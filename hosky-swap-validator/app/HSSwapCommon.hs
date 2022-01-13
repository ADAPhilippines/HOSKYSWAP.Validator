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


module HSSwapCommon
    ( SwapInfo (..)
    , price
    , (&&&)
    , (|||)
    , ContractInfo (..)
    , contractInfo
    ) where

import              Data.Aeson (FromJSON, ToJSON)
import              GHC.Generics
import              Ledger
import qualified    PlutusTx
import              PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified    PlutusTx.Prelude as P
import qualified    Prelude as Pr (Show (..), Eq)

data SwapInfo = SwapInfo
    { siRate       :: !Integer
    , siFromAsset  :: !AssetClass
    , siToAsset    :: !AssetClass
    , siSeller     :: !PubKeyHash
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

instance Eq SwapInfo where
    {-# INLINABLE (==) #-}
    a == b = (siRate         a == siRate          b) &&
             (siFromAsset    a == siFromAsset     b) &&
             (siToAsset      a == siToAsset       b) &&
             (siSeller       a == siSeller        b)

PlutusTx.makeIsDataIndexed ''SwapInfo [('SwapInfo, 0)]

data ContractInfo = ContractInfo
    { ciAdminPKH        :: !PubKeyHash
    , ciRugPullFee      :: !Integer
    , ciMinUtxoLovelace :: !Integer
    , ciSellerFeeShare  :: !Integer
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 0)]
PlutusTx.makeLift ''ContractInfo

contractInfo :: ContractInfo
contractInfo = ContractInfo
    { ciAdminPKH        = "eeab87f40a94ac5276ae70b39b50726d15ac3bb5defab7e3bf4794a8"
    , ciRugPullFee      = 694_200
    , ciMinUtxoLovelace = 2_000_000
    , ciSellerFeeShare  = 300_000
    }

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price amt exchangeRate = (amt P.* exchangeRate) `P.divide` 1_000_000 -- allow for six decimal places for exchange rate e.g. 1.000000

{-# INLINABLE (|||) #-}
(|||) :: Bool -> Bool -> Bool
(|||) x y = if x then True else y

{-# INLINABLE (&&&) #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False