{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
-- This example is taken directly from cardano-api, written by Jordan Millar, IOHK

module HSSwapValidator
    ( hsSwapSerialised
    , hsSwapSBS
    , hsSwapAddress
    , hsSwapInstance
    , hsSwapValidator
    , HSSwap
    ) where

import              Cardano.Api.Shelley     (PlutusScript (..))
import              Codec.Serialise
import qualified    Data.ByteString.Lazy    as LBS
import qualified    Data.ByteString.Short   as SBS
import              Ledger
import              Ledger.Value            as Value
import qualified    PlutusTx.Prelude        as P
import qualified    Ledger.Typed.Scripts    as Scripts
import qualified    PlutusTx
import              PlutusTx.Prelude hiding (Semigroup (..), unless)
import              Cardano.Api             ( PlutusScriptV1 )
import              HSSwapCommon
import qualified    Plutus.V1.Ledger.Ada as Ada

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> SwapInfo -> BuiltinData -> ScriptContext -> P.Bool
mkValidator ContractInfo{..} d _ ctx =
    traceIfFalse    "Only 1 script input allowed"           hasOneScriptInput           &&&
    traceIfFalse    "Fees not paid"                         isFeePaid                   &&&
    (                                                       hasSellerSigned             |||
    (traceIfFalse   "Min Utxo Lovelace not returned"        isMinUtxoLovelaceReturned   &&&
     traceIfFalse   "Seller not paid"                       isSellerPaid ))

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasSellerSigned :: Bool
        hasSellerSigned = txSignedBy info $ siSeller d

        minPrice :: Integer
        minPrice =
            let
                fromAssetAmt = case findOwnInput ctx of
                    Nothing -> traceError "Own input not found"
                    Just i  -> assetClassValueOf (txOutValue $ txInInfoResolved i) (siFromAsset d)
            in
                if   doesSellerOfferLovelace
                then price (fromAssetAmt P.- ciRugPullFee P.- ciMinUtxoLovelace P.- ciSellerFeeShare) (siRate d)
                else price fromAssetAmt (siRate d)

        isSellerPaid :: Bool
        isSellerPaid =
            let
                pricePaid :: Integer
                pricePaid =  assetClassValueOf (valuePaidTo info $ siSeller d) (siToAsset d)

                additional :: Integer
                additional = if doesSellerWantLovelace then ciMinUtxoLovelace else 0
            in
                pricePaid P.>= minPrice + additional

        doesSellerOfferLovelace :: Bool
        doesSellerOfferLovelace = siFromAsset d P.== AssetClass (Ada.adaSymbol, Ada.adaToken)

        doesSellerWantLovelace :: Bool
        doesSellerWantLovelace = siToAsset d P.== AssetClass (Ada.adaSymbol, Ada.adaToken)

        isMinUtxoLovelaceReturned :: Bool
        isMinUtxoLovelaceReturned = ciMinUtxoLovelace P.<= Ada.getLovelace (Ada.fromValue $ valuePaidTo info (siSeller d))
        
        isFeePaid :: Bool
        isFeePaid =
            let
                pricePaid :: Integer
                pricePaid = Ada.getLovelace $ Ada.fromValue $ valuePaidTo info ciAdminPKH
            in
                pricePaid P.>= 2 P.* ciRugPullFee
        
        hasOneScriptInput :: Bool
        hasOneScriptInput = 
            let
                ownInputs :: [TxInInfo]
                ownInputs = P.filter ((== Just (ownHash ctx)) . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
            in
                P.length ownInputs == 1
                
{-
    As a ScriptInstance
-}
data HSSwap
instance Scripts.ValidatorTypes HSSwap where
    type instance DatumType HSSwap = SwapInfo
    type instance RedeemerType HSSwap = BuiltinData

hsSwapInstance :: ContractInfo -> Scripts.TypedValidator HSSwap
hsSwapInstance ci = Scripts.mkTypedValidator @HSSwap
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ci)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SwapInfo @BuiltinData


hsSwapValidator :: Validator
hsSwapValidator = Scripts.validatorScript $ hsSwapInstance contractInfo 

hsSwapScript :: Ledger.Script
hsSwapScript = unValidatorScript hsSwapValidator

hsSwapSBS :: SBS.ShortByteString
hsSwapSBS =  SBS.toShort . LBS.toStrict $ serialise hsSwapScript

hsSwapSerialised :: PlutusScript PlutusScriptV1
hsSwapSerialised = PlutusScriptSerialised hsSwapSBS

hsSwapAddress :: Ledger.Address
hsSwapAddress = scriptAddress hsSwapValidator



