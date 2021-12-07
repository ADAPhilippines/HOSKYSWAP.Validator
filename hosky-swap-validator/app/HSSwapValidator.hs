{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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
import              PlutusTx.Skeleton

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> SwapInfo -> BuiltinData -> ScriptContext -> P.Bool
mkValidator ci d _ ctx =
    hasSellerSigned |||
    (traceIfFalse   "Cannot have more than 1 validator" hasOneValidatorInput &&&
    traceIfFalse    "Seller not paid"                   isSellerPaid &&&
    traceIfFalse    "Fees not paid"                     isFeePaid )

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        rugPullFee :: Integer
        rugPullFee = ciRugPullFee ci

        minUtxoLovelace :: Integer
        minUtxoLovelace = ciMinUtxoLovelace ci

        adminPKH :: PubKeyHash
        adminPKH = ciAdminPKH ci

        hasSellerSigned :: Bool
        hasSellerSigned = txSignedBy info $ siSeller d

        minPrice :: Integer
        minPrice =
            let
                fromAssetAmt = case findOwnInput ctx of
                    Nothing -> traceError "Own input not found"
                    Just i  -> assetClassValueOf (txOutValue $ txInInfoResolved i) (siFromAsset d)
            in
                price fromAssetAmt (siRate d)

        isSellerPaid :: Bool
        isSellerPaid =
            let
                pricePaid :: Integer
                pricePaid =  assetClassValueOf (valuePaidTo info $ siSeller d) (siToAsset d)
            in
                if   doesSellerWantLovelace
                then traceIfFalse "lovelace payment amount insufficient"    (pricePaid P.>= (minPrice P.- rugPullFee))
                else traceIfFalse "Asset amount insufficient"               (pricePaid P.>= minPrice) &&&
                     traceIfFalse "Min utxo lovelace amount insufficient"   isMinUtxoLovelaceEnsured

        doesSellerWantLovelace :: Bool
        doesSellerWantLovelace = siToAsset d P.== AssetClass (Ada.adaSymbol, Ada.adaToken)

        isMinUtxoLovelaceEnsured :: Bool
        isMinUtxoLovelaceEnsured = minUtxoLovelace  P.<= Ada.getLovelace (Ada.fromValue $ valuePaidTo info (siSeller d))
        
        isFeePaid :: Bool
        isFeePaid =
            let
                pricePaid :: Integer
                pricePaid = Ada.getLovelace $ Ada.fromValue $ valuePaidTo info adminPKH
            in
                pricePaid P.>= 2 P.* rugPullFee

        hasOneValidatorInput :: Bool
        hasOneValidatorInput =
            let
                validatorInputs = P.filter (P.isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
            in
                P.length validatorInputs P.== 1
                
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



