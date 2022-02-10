{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-| Generators for the property tests
-}
module Convex.Options.Generators(
  -- * Generators for @convex-options@ types
  optionType,
  optionState,
  option,

  -- * Generators for @Ledger@ types
  tokenName,
  currencySymbol,
  assetClass,
  nonNegativeAda,
  wallet
  ) where

import Convex.Options.OnChain.Types (Option (..), OptionState (..), OptionType (..))
import Cooked.MockChain.Wallet (Wallet)
import Cooked.MockChain.Wallet qualified as W
import Plutus.V1.Ledger.Ada (Ada (..))
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol, TokenName)
import Plutus.V1.Ledger.Value qualified as V
import Test.QuickCheck qualified as QC

optionType :: QC.Gen OptionType
optionType = QC.elements [Put, Call]

optionState :: QC.Gen OptionState
optionState = QC.elements [Ready, Exercised]

currencySymbol :: QC.Gen CurrencySymbol
currencySymbol = QC.elements ["00ff", "aabb"]

tokenName :: QC.Gen TokenName
tokenName = QC.elements ["a", "b", "c", "My token", "DOGECOIN", "BUYER", "SELLER"]

assetClass :: QC.Gen AssetClass
assetClass = V.assetClass <$> currencySymbol <*> tokenName

nonNegativeAda :: QC.Gen Ada
nonNegativeAda = Lovelace <$> QC.arbitrarySizedNatural

wallet :: QC.Gen Wallet
wallet = QC.elements W.knownWallets

option :: QC.Gen Option
option =
  Option
    <$> assetClass
    <*> (fromIntegral @Int <$> QC.chooseBoundedIntegral (0, 10_000_000_000))
    <*> nonNegativeAda
    <*> (Lovelace . fromIntegral @Int <$> QC.chooseBoundedIntegral (2_000_000, 20_000_000))
    <*> optionType
    <*> pure 0 -- FIXME
    <*> pure 10_000
    <*> pure "BUYER"
    <*> pure "SELLER"
    <*> pure "UTXO"
