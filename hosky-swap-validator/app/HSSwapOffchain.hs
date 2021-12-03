{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

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


import              Prelude                 (Semigroup (..), Show (..), String)
import              Control.Monad           hiding (fmap)
import              Data.Aeson              (ToJSON, FromJSON)
import              GHC.Generics
import              Data.Text               (Text)
import qualified    Data.Map                as Map


import              HSSwapValidator
import              HSSwapCommon
import Plutus.V1.Ledger.Ada (lovelaceValueOf)


hsSwapDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe SwapInfo
hsSwapDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

offerSwap :: (AsContractError e) => OfferSwapParams -> Contract w s e ()
offerSwap params = do
    let swap = hsSwap params
        amt = amount params
        tx = Constraints.mustPayToTheScript swap $ assetClassValue (sFromAsset swap) amt
    ledgerTx <- submitTxConstraints hsSwapInstance tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " P.++ show amt P.++ " for swap"


-- TODO figure out a way to convert datum to hash
findSwaps :: (AsContractError e) => Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, SwapInfo))
findSwaps = do
    utxos <- utxosTxOutTxAt hsSwapAddress
    return $ case [head $ Map.toList utxos] of
        [(oref, (o, citx))] -> do
            x <- hsSwapDatum (toTxOut o) $ \dh -> Map.lookup dh $ _citxData citx
            return (oref, o, x)

executeSwap :: (AsContractError e) => Contract w s e ()
executeSwap = do
    foundSwaps <- findSwaps
    case foundSwaps of
        Nothing           -> logInfo @String "swap not found"
        Just (oref, o, d) -> do
            let p       =   assetClassValue (sToAsset d) $ price (assetClassValueOf (txOutValue $ toTxOut o) (sFromAsset d)) (sRate d)
                feeValue     =   lovelaceValueOf (2 * 69420)
                lookups =   Constraints.otherScript hsSwapValidator                                     
                            <> Constraints.unspentOutputs (Map.fromList [(oref, o)])
                tx      =   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ()) 
                            <> Constraints.mustPayToPubKey (sSeller d) p
                            <> Constraints.mustPayToPubKey adminPKH feeValue
            ledgerTx <- submitTxConstraintsWith @HSSwap lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "made swap with price " P.++ show (Value.flattenValue p)

cancelSwap :: (AsContractError e) => Contract w s e ()
cancelSwap = do
    foundSwaps <- findSwaps
    case foundSwaps of
        Nothing           -> logInfo @String "swap not found"
        Just (oref, o, _) -> do
            let lookups =   Constraints.otherScript hsSwapValidator                                     
                            <> Constraints.unspentOutputs (Map.fromList [(oref, o)])
                tx      =   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ())
            ledgerTx <- submitTxConstraintsWith @HSSwap lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "canceled swap"

cancelAllSwaps :: (AsContractError e) => Contract w s e ()
cancelAllSwaps = do
    utxos <- utxosAt hsSwapAddress
    case Map.toList utxos of
        []              -> logInfo @String "no utxos at the script address"
        _               -> do
            let     lookups =   Constraints.unspentOutputs utxos
                            <> Constraints.otherScript hsSwapValidator
                    tx      =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ()) | (oref, _) <- Map.toList utxos]
            ledgerTx <- submitTxConstraintsWith @HSSwap lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "canceled swap"


cheatSwap :: (AsContractError e) => Contract w s e ()
cheatSwap = do
    foundSwaps <- findSwaps
    case foundSwaps of
        Nothing           -> logInfo @String "swap not found"
        Just (oref, o, d) -> do
            let p       =   assetClassValue (sToAsset d) $ price (assetClassValueOf (txOutValue $ toTxOut o) (sFromAsset d)) (sRate d) - 2
                lookups =   Constraints.otherScript hsSwapValidator                                     
                            <> Constraints.unspentOutputs (Map.fromList [(oref, o)])
                tx      =   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ())
            ledgerTx <- submitTxConstraintsWith @HSSwap lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "made swap with price " P.++ show (Value.flattenValue p)


data OfferSwapParams = OfferSwapParams
    { hsSwap    ::  !SwapInfo
    , amount    ::  !Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

type HSSwapSchema =
            Endpoint "offer" OfferSwapParams
        .\/ Endpoint "execute" ()
        .\/ Endpoint "cancel" ()
        .\/ Endpoint "cancelAll" ()
        .\/ Endpoint "cheat" ()

endpoints :: Contract () HSSwapSchema Text ()
endpoints = forever
    $ awaitPromise
    $ offer' `select` execute' `select` cancel' `select` cancelAll' `select` cheat'
  where
    offer' = endpoint @"offer" $ \params -> offerSwap params
    execute' = endpoint @"execute" $ const executeSwap
    cancel' = endpoint @"cancel" $ const cancelSwap
    cancelAll' = endpoint @"cancelAll" $ const cancelAllSwaps
    cheat' = endpoint @"cheat" $ const cheatSwap