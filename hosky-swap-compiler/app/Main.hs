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

import            Prelude
import            System.Environment

import            Cardano.Api
import            Plutus.V1.Ledger.Contexts
import qualified  Plutus.V1.Ledger.Api as Plutus
import qualified  PlutusTx.Prelude as P
import qualified  PlutusTx
import qualified  Data.ByteString.Short as SBS
import            HSSwapCommon
import            HSSwapValidator
import            Data.String (IsString(fromString))
import            PlutusTx.Builtins.Class (stringToBuiltinByteString)
import            Plutus.V1.Ledger.Crypto
import            Plutus.V1.Ledger.Value
import            Data.Aeson
import Cardano.Api.Shelley ( fromPlutusData )
import GHC.Generics

data SimpleDatum = SimpleDatum
    { 
      sdTest :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

data TestDatum = TestDatum
    {   tdRate              :: !Integer
    ,   tdTest              :: !SimpleDatum
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''SimpleDatum [('SimpleDatum, 1)]
PlutusTx.makeIsDataIndexed ''TestDatum [('TestDatum, 1)]

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = "./scripts/" ++ if nargs > 0 then head args else  "result.plutus"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusScript scriptname hsSwapSerialised


writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> IO ()
writePlutusScript filename scriptSerial =
  do
  let td = TestDatum {
    tdRate = 1000000,
    tdTest = SimpleDatum {sdTest = 1}
  }
  let si = SwapInfo {
    sRate = 1000000,
    sFromAsset = AssetClass ("", ""),
    sToAsset = AssetClass ("88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de", "HOSKY"),
    sSeller = "945c010ec1c1f884ed778c28b3c644dcc2da1c3b1df4a90924cc51de"
  }
  print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData si))
  print $ "Redeemer value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData ()))
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()