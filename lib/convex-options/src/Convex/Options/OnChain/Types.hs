{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}

module Convex.Options.OnChain.Types(
  OptionType(..),
  Option(..),
  OptionParam(..),
  OptionRedeemer(..),
  OptionState(..),
  MPSRedeemer(..),
  optionValue,
  optionValueLocked,
  exerciseInterval,
  reclaimInterval,
  initialParam
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (AssetClass, CurrencySymbol, POSIXTime (..), POSIXTimeRange, TokenName, Value)
import Plutus.V1.Ledger.Ada (Ada)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as V
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified

data OptionType = Put | Call
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''OptionType
PlutusTx.makeIsDataIndexed ''OptionType [('Put,0), ('Call, 1)]

data OptionState = Ready | Exercised
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''OptionState
PlutusTx.makeIsDataIndexed ''OptionState [('Ready,0), ('Exercised, 1)]

data Option =
  Option
    { opAsset            :: !AssetClass -- ^ Asset to be bought or sold
    , opAmount           :: !Integer -- ^ Quantity of the asset
    , opStrikePrice      :: !Ada -- ^ Strike price
    , opDeposit          :: !Ada
    , opType             :: !OptionType -- ^ Type of option
    , opDate             :: !POSIXTime -- ^ Expiration date
    , opRedemptionPeriod :: !Integer -- ^ Time (in milliseconds) that can pass after 'opDate' before the seller of the option can reclaim the output.
    , opBuyerTN          :: !TokenName -- ^ Token name for the BUYER token
    , opSellerTN         :: !TokenName -- ^ Token name for the SELLER token
    }
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Option
PlutusTx.makeIsDataIndexed ''Option [('Option,0)]

data OptionParam =
  OptionParam
    { oppOption   :: !Option
    , oppState    :: !OptionState
    , oppCurrency :: !CurrencySymbol
    }
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''OptionParam
PlutusTx.makeIsDataIndexed ''OptionParam [('OptionParam,0)]

{-| Value that must be locked in the option output, *excluding*
the minimum deposit!
-}
{-# INLINABLE optionValue #-}
optionValue :: OptionState -> Option -> Value
optionValue Ready Option{opType=Put, opStrikePrice}         = Ada.toValue opStrikePrice
optionValue Exercised Option{opType=Put, opAsset, opAmount} = V.assetClassValue opAsset opAmount
optionValue Ready Option{opType=Call, opAsset, opAmount}    = V.assetClassValue opAsset opAmount
optionValue Exercised Option{opType=Call, opStrikePrice}    = Ada.toValue opStrikePrice

{-# INLINABLE initialParam #-}
initialParam :: CurrencySymbol -> Option -> OptionParam
initialParam oppCurrency oppOption =
  OptionParam
    { oppOption
    , oppCurrency
    , oppState = Ready
    }

{-| Value that must be locked by the option output.
-}
{-# INLINABLE optionValueLocked #-}
optionValueLocked :: OptionState -> Option -> Value
optionValueLocked s o@Option{opDeposit} = Ada.toValue opDeposit <> optionValue s o

{-# INLINABLE exerciseInterval #-}
exerciseInterval :: Option -> POSIXTimeRange
exerciseInterval Option{opDate} = Interval.from opDate

{-# INLINABLE reclaimInterval #-}
reclaimInterval :: Option -> POSIXTimeRange
reclaimInterval Option{opDate, opRedemptionPeriod} = Interval.from (opDate + POSIXTime opRedemptionPeriod)

data OptionRedeemer =
  Exercise -- ^ Exercise the option. Can be performed by the holder of the 'buyer' token
  | Reclaim -- ^ Reclaim the output. Can be performed by the holder of the 'seller' token
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''OptionRedeemer
PlutusTx.makeIsDataIndexed ''OptionRedeemer [('Exercise,0), ('Reclaim, 1)]

data MPSRedeemer
  = MPSMinting -- ^ For creating the token
  | MPSBurning -- ^ For burning the token
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq MPSRedeemer where
  {-# INLINABLE (==) #-}
  MPSMinting == MPSMinting = True
  MPSBurning == MPSBurning = True
  _ == _                   = False

PlutusTx.makeLift ''MPSRedeemer
PlutusTx.makeIsDataIndexed ''MPSRedeemer [('MPSMinting,0), ('MPSBurning, 1)]
