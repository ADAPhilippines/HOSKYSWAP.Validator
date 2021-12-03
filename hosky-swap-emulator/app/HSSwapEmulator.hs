{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HSSwapEmulator
    (runEmulator
    , test
    , getPKH
    ) where



import              PlutusTx.Prelude        as Plutus
import              Plutus.Trace.Emulator   as Emulator
import              Plutus.V1.Ledger.Ada    (lovelaceValueOf)
import              Wallet.Emulator.Wallet  as Wallet
import              Ledger
import              Ledger.Value            as Value
import qualified    Ledger.Ada              as Ada
import              Data.Default            (def)

import              Prelude                 (IO, print)
import qualified    Data.Map                as Map
import              Control.Monad           (void)

import              HSSwapOffchain
import              HSSwapCommon

lovelaceTN :: TokenName
lovelaceTN = TokenName ""

lovelaceCS :: CurrencySymbol
lovelaceCS = CurrencySymbol ""

lovelaceAsset :: AssetClass
lovelaceAsset = AssetClass (lovelaceCS, lovelaceTN)

dummyTN :: TokenName
dummyTN = TokenName "HOSKY"

dummyCS :: CurrencySymbol
dummyCS = CurrencySymbol "ff"

dummyAsset :: AssetClass
dummyAsset = AssetClass (dummyCS, dummyTN)

wallet :: Integer -> Wallet
wallet = fromWalletNumber . WalletNumber

runEmulator :: IO()
runEmulator = do
    runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList [(wallet i, v) | i <- [1..3]]) def def

    v :: Value
    v = Ada.lovelaceValueOf                 100_000_000 <>
        Value.singleton dummyCS dummyTN     100_000_000

    myTrace :: EmulatorTrace ()
    myTrace = do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        void $ Emulator.waitNSlots 3
        callEndpoint @"offer" h1 OfferSwapParams
            {hsSwap = SwapInfo
                    { sRate = 1_000_000
                    , sFromAsset = dummyAsset
                    , sToAsset = lovelaceAsset
                    , sSeller = pubKeyHash $ Wallet.walletPubKey $ wallet 1
                    }
            , amount = 50_000_000
            } 
        void $ Emulator.waitNSlots 3
        callEndpoint @"execute" h2 ()
        void $ Emulator.waitNSlots 3

test :: Value
test = lovelaceValueOf 2 Plutus.<> Value.singleton lovelaceCS lovelaceTN 5

getPKH :: Integer -> PubKeyHash
getPKH = pubKeyHash . Wallet.walletPubKey . wallet