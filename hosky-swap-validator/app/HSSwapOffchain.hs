{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings #-}

module HSSwapOffchain
    ( endpoints
    , OfferSwapParams (..)
    ) where


import              PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified    PlutusTx.Prelude        as P
import qualified    PlutusTx
import              Plutus.Contract         as Contract
import              Ledger.Constraints      as Constraints
import              Ledger
import              Ledger.Value            as Value
import              Plutus.ChainIndex.Tx
import              Plutus.V1.Ledger.Ada


import              Prelude                 (Semigroup (..), Show (..), String)
import              Control.Monad           hiding (fmap)
import              Data.Aeson              (ToJSON, FromJSON)
import              GHC.Generics
import              Data.Text               (Text)
import qualified    Data.Map                as Map


import              HSSwapValidator
import              HSSwapCommon


hsSwapDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe SwapInfo
hsSwapDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

offerSwap :: (AsContractError e) => OfferSwapParams -> Contract w s e ()
offerSwap params = do
    let swap            =   hsSwap params
        amt             =   amount params
        minUtxoLovelace =   ciMinUtxoLovelace contractInfo
        additional      =   if siFromAsset swap P./= AssetClass (adaSymbol, adaToken) then minUtxoLovelace else 0
        totalValue      =   assetClassValue (siFromAsset swap) amt <> lovelaceValueOf additional
        tx              =   Constraints.mustPayToTheScript swap totalValue
    ledgerTx <- submitTxConstraints (hsSwapInstance contractInfo) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " P.++ show (Value.flattenValue totalValue) P.++ " for " P.++ show (siToAsset swap)

findSwaps :: (AsContractError e) => SwapInfo -> Contract w s e [Maybe (TxOutRef, ChainIndexTxOut, SwapInfo)]
findSwaps si = do
    utxos <- utxosTxOutTxAt hsSwapAddress
    return $ getValidUtxos si $ Map.toList utxos


getValidUtxos :: SwapInfo -> [(TxOutRef,(ChainIndexTxOut, ChainIndexTx))] -> [Maybe (TxOutRef, ChainIndexTxOut, SwapInfo)]
getValidUtxos _ []     = []
getValidUtxos d utxos  = swaps
  where swaps                         = [ op | utxo <- utxos, let op = getOutputs utxo, P.isJust op ]
        getOutputs (oref, (o, citx))  = do
            x <- hsSwapDatum (toTxOut o) $ \dh -> Map.lookup dh $ _citxData citx
            if x == d
            then return (oref, o, x)
            else Nothing

executeSwap :: (AsContractError e) => SwapInfo -> Contract w s e ()
executeSwap si = do
    foundSwaps <- findSwaps si
    case foundSwaps of
        [] -> logInfo @String "No swaps found"
        xs -> do
            case head xs of
                Nothing             -> logInfo @String "swap not found"
                Just (oref, o, d)   -> do
                    let adminPKH            =   ciAdminPKH contractInfo
                        rugPullFee          =   ciRugPullFee contractInfo
                        minUtxoLovelace     =   ciMinUtxoLovelace contractInfo
                        additionalLovelace  =   if siToAsset d == AssetClass(adaSymbol, adaToken) then (-rugPullFee) else minUtxoLovelace
                        swapPrice           =   price (assetClassValueOf (txOutValue $ toTxOut o) (siFromAsset d)) (siRate d)
                        p                   =   assetClassValue (siToAsset d) swapPrice                                         <>
                                                lovelaceValueOf additionalLovelace
                        feeValue            =   lovelaceValueOf $ 2 * rugPullFee
                        lookups             =   Constraints.otherScript hsSwapValidator                                         <>
                                                Constraints.unspentOutputs (Map.fromList [(oref, o)])
                        tx                  =   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ())   <>
                                                Constraints.mustPayToPubKey (siSeller d) p                                      <>
                                                Constraints.mustPayToPubKey adminPKH feeValue
                    logInfo @String $ "Paying" P.++ show (Value.flattenValue p) P.++ "to the Seller"
                    logInfo @String $ "Paying swap fee of" P.++ show (Value.flattenValue feeValue)
                    ledgerTx <- submitTxConstraintsWith @HSSwap lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " P.++ show (Value.flattenValue p)

cancelSwap :: (AsContractError e) => SwapInfo -> Contract w s e ()
cancelSwap si = do
    foundSwaps <- findSwaps si
    case foundSwaps of
        [] -> logInfo @String "No swaps found"
        xs -> do
            case head xs of
                Nothing           -> logInfo @String "swap not found"
                Just (oref, o, _) -> do
                    let lookups     =   Constraints.otherScript hsSwapValidator <>
                                        Constraints.unspentOutputs (Map.fromList [(oref, o)])
                        tx          =   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ())
                    ledgerTx <- submitTxConstraintsWith @HSSwap lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "canceled swap"

cancelAllSwaps :: (AsContractError e) => Contract w s e ()
cancelAllSwaps = do
    utxos <- utxosAt hsSwapAddress
    case Map.toList utxos of
        []  -> logInfo @String "no utxos at the script address"
        _   -> do
            let lookups =   Constraints.unspentOutputs utxos <>
                            Constraints.otherScript hsSwapValidator
                tx      =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ()) | (oref, _) <- Map.toList utxos]
            ledgerTx <- submitTxConstraintsWith @HSSwap lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "canceled swap"


cheatSwap :: (AsContractError e) => SwapInfo -> Contract w s e ()
cheatSwap si = do
    foundSwaps <- findSwaps si
    case foundSwaps of
        [] -> logInfo @String "No swaps found"
        xs -> do
            case head xs of
                Nothing           -> logInfo @String "swap not found"
                Just (oref, o, d) -> do
                    let p       =   assetClassValue (siToAsset d) $ price (assetClassValueOf (txOutValue $ toTxOut o) (siFromAsset d)) (siRate d) - 2
                        lookups =   Constraints.otherScript hsSwapValidator  <>
                                    Constraints.unspentOutputs (Map.fromList [(oref, o)])
                        tx      =   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ())
                    ledgerTx <- submitTxConstraintsWith @HSSwap lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "cheated swap with price " P.++ show (Value.flattenValue p)


data OfferSwapParams = OfferSwapParams
    { hsSwap    ::  !SwapInfo
    , amount    ::  !Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

type HSSwapSchema =
            Endpoint "offer" OfferSwapParams
        .\/ Endpoint "execute" SwapInfo
        .\/ Endpoint "cancel" SwapInfo
        .\/ Endpoint "cancelAll" ()
        .\/ Endpoint "cheat" SwapInfo

endpoints :: Contract () HSSwapSchema Text ()
endpoints = forever
    $ awaitPromise
    $ offer' `select` execute' `select` cancel' `select` cancelAll' `select` cheat'
  where
    offer' = endpoint @"offer" $ \params -> offerSwap params
    execute' = endpoint @"execute" $ \params -> executeSwap params
    cancel' = endpoint @"cancel" $ \params -> cancelSwap params
    cancelAll' = endpoint @"cancelAll" $ const cancelAllSwaps
    cheat' = endpoint @"cheat" $ \params -> cheatSwap params