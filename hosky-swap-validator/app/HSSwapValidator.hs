{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

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
mkValidator :: SwapInfo -> BuiltinData -> ScriptContext -> P.Bool
mkValidator d _ ctx =
    hasSellerSigned |||
    -- ( traceIfFalse "Cannot have more than 1 validator" hasOneValidatorInput &&&
    --   traceIfFalse "Seller not paid" isSellerPaid &&&
    --   traceIfFalse "Fees not paid" isFeePaid )

    ( traceIfFalse "Cannot have more than 1 validator" hasOneValidatorInput &&&
      traceIfFalse "Seller not paid" isSellerPaid )
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasSellerSigned :: Bool
        hasSellerSigned = txSignedBy info $ sSeller d

        minPrice :: Integer
        minPrice =
            let
                fromAssetAmt = case findOwnInput ctx of
                    Nothing -> traceError "Own input not found"
                    Just i  -> assetClassValueOf (txOutValue $ txInInfoResolved i) (sFromAsset d)
            in
                price fromAssetAmt (sRate d)

        isSellerPaid :: Bool
        isSellerPaid =
            let
                pricePaid :: Integer
                pricePaid =  assetClassValueOf (valuePaidTo info $ sSeller d) (sToAsset d)
            in
                traceIfFalseSkeletal pricePaid False ||| traceIfFalseSkeletal (getPubKeyHash $ sSeller d) False ||| (pricePaid P.>= minPrice)
        
        isFeePaid :: Bool
        isFeePaid =
            let
                pricePaid :: Integer
                pricePaid = Ada.getLovelace $ Ada.fromValue $ valuePaidTo info adminPKH
            in
                pricePaid P.>= 2 * 694200

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

hsSwapInstance :: Scripts.TypedValidator HSSwap
hsSwapInstance = Scripts.mkTypedValidator @HSSwap
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SwapInfo @BuiltinData


hsSwapValidator :: Validator
hsSwapValidator = Scripts.validatorScript hsSwapInstance

hsSwapScript :: Ledger.Script
hsSwapScript = unValidatorScript hsSwapValidator

hsSwapSBS :: SBS.ShortByteString
hsSwapSBS =  SBS.toShort . LBS.toStrict $ serialise hsSwapScript

hsSwapSerialised :: PlutusScript PlutusScriptV1
hsSwapSerialised = PlutusScriptSerialised hsSwapSBS

hsSwapAddress :: Ledger.Address
hsSwapAddress = scriptAddress hsSwapValidator


