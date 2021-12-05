{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module HSSwapCommon
  ( SwapInfo (..),
    price,
    (&&&),
    (|||),
    adminPKH,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Ledger
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.Prelude as P
import Prelude (Show (..))

data SwapInfo = SwapInfo
  { sRate       :: !Integer,
    sFromAsset  :: !AssetClass,
    sToAsset    :: !AssetClass,
    sSeller     :: !PubKeyHash
  }
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''SwapInfo [('SwapInfo, 0)]

{-# INLINEABLE price #-}
price :: Integer -> Integer -> Integer
price amt exchangeRate = (amt P.* exchangeRate) `P.divide` 1_000_000 -- allow for six decimal places for exchange rate e.g. 1.000000

-- {-# INLINABLE ||| #-}
(|||) :: Bool -> Bool -> Bool
(|||) x y = if x then True else y

-- {-# INLINABLE &&& #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False

adminPKH :: PubKeyHash
adminPKH = "6ad510fe5e2eff4f367475f01ab79dc4cd1f2600bda02ab270577637" --wallet 3 pubKeyHash
