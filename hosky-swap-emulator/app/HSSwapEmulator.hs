{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HSSwapEmulator
    ( runEmulator
    , getPKH
    ) where



import              PlutusTx.Prelude        as Plutus
import              Plutus.Trace.Emulator   as Emulator
import              Wallet.Emulator.Wallet  as Wallet
import              Ledger
import              Ledger.Value            as Value
import qualified    Ledger.Ada              as Ada

import qualified    Prelude                 as Haskell
import              Data.Default            (def)
import qualified    Data.Map                as Map
import              Control.Monad           (void)

import              HSSwapOffchain
import              HSSwapCommon

lovelaceTN :: TokenName
lovelaceTN = ""

lovelaceCS :: CurrencySymbol
lovelaceCS = ""

lovelaceAsset :: AssetClass
lovelaceAsset = AssetClass (lovelaceCS, lovelaceTN)

dummyTN :: TokenName
dummyTN = "HOSKY"

dummyCS :: CurrencySymbol
dummyCS = "ff"

dummyAsset :: AssetClass
dummyAsset = AssetClass (dummyCS, dummyTN)

dummyTN2 :: TokenName
dummyTN2 = "USDT"

dummyCS2 :: CurrencySymbol
dummyCS2 = "ee"

dummyAsset2 :: AssetClass
dummyAsset2 = AssetClass (dummyCS2, dummyTN2)

wallet :: Integer -> Wallet
wallet = fromWalletNumber . WalletNumber

swap1 :: SwapInfo -- trading ADA to HOSKY at 0.00000095 $ADA/$HOSKY
swap1 = SwapInfo  { siRate = 1_000_000
                  , siFromAsset = lovelaceAsset
                  , siToAsset = dummyAsset
                  , siSeller = pubKeyHash $ Wallet.walletPubKey $ wallet 1
                  }

swap2 :: SwapInfo -- trading HOSKY to ADA at 1 ADA / 1_000_000 HOSKY
swap2 = SwapInfo  { siRate = 1_000_000
                  , siFromAsset = dummyAsset
                  , siToAsset = lovelaceAsset
                  , siSeller = pubKeyHash $ Wallet.walletPubKey $ wallet 1
                  }

swap3 :: SwapInfo -- trading HOSKY to HOSKY2 at 1 HOSKY / 1 HOSKY2
swap3 = SwapInfo  { siRate = 1_000_000
                  , siFromAsset = dummyAsset
                  , siToAsset = dummyAsset2
                  , siSeller = pubKeyHash $ Wallet.walletPubKey $ wallet 1
                  }

runEmulator :: Haskell.IO()
runEmulator = do
    runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList [(wallet i, v) | i <- [1..3]]) def def

    v :: Value
    v = Ada.lovelaceValueOf                     15_000_000 <>
        Value.singleton dummyCS dummyTN         15_000_000 <>
        Value.singleton dummyCS2 dummyTN2       15_000_000

    myTrace :: EmulatorTrace ()
    myTrace = do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"offer" h1 OfferSwapParams
            { hsSwap = swap1
            , amount = 10_000_000
            }
        -- void $ Emulator.waitNSlots 1
        -- callEndpoint @"offer" h1 OfferSwapParams
        --     { hsSwap = swap2
        --     , amount = 10_000_000
        --     }
        void $ Emulator.waitNSlots 1
        callEndpoint @"execute" h2 swap1
        void $ Emulator.waitNSlots 1

getPKH :: Integer -> PubKeyHash
getPKH = pubKeyHash . Wallet.walletPubKey . wallet