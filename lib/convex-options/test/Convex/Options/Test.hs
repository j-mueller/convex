{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module Convex.Options.Test(optionTests) where

import Convex.Options.Generators qualified as Gen
import Convex.Options.OffChain qualified as Option
import Convex.Options.OnChain.Types (Option (..))
import Cooked.MockChain.Monad (as)
import Cooked.MockChain.Testing (testSucceedsFrom)
import Cooked.MockChain.Wallet (InitialDistribution, Wallet)
import Cooked.MockChain.Wallet qualified as W
import Plutus.V1.Ledger.Value qualified as V
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

optionTests :: TestTree
optionTests = testGroup "option-success-flows" -- Test the normal operation of the option contract
  [ option_initialise
  , option_exercise
  , option_exercise_reclaim
  , option_reclaim
  ]

option_initialise :: TestTree
option_initialise =
  let gen = (,) <$> Gen.option <*> Gen.wallet in
  testProperty "initialise" $
    QC.forAll gen $ \(opt, wallet) ->
      testSucceedsFrom @QC.Property (initialDist opt wallet) $ do
        Option.initialise opt `as` wallet

option_exercise :: TestTree
option_exercise =
  let gen = (,) <$> Gen.option <*> Gen.wallet in
  testProperty "initialise-exercise" $
    QC.forAll gen $ \(opt, wallet) ->
      testSucceedsFrom @QC.Property (initialDist opt wallet) $ do
        (Option.initialise opt >>= Option.exercise) `as` wallet

option_exercise_reclaim :: TestTree
option_exercise_reclaim =
  let gen = (,) <$> Gen.option <*> Gen.wallet in
  testProperty "initialise-exercise-reclaim" $
    QC.forAll gen $ \(opt, wallet) ->
      testSucceedsFrom @QC.Property (initialDist opt wallet) $ do
        (Option.initialise opt >>= Option.exercise >>= Option.reclaim) `as` wallet

option_reclaim :: TestTree
option_reclaim =
  let gen = (,) <$> Gen.option <*> Gen.wallet in
  testProperty "initialise-reclaim" $
    QC.forAll gen $ \(opt, wallet) ->
      testSucceedsFrom @QC.Property (initialDist opt wallet) $ do
        (Option.initialise opt >>= Option.reclaim) `as` wallet

initialDist :: Option -> Wallet -> InitialDistribution
initialDist Option{opAsset, opAmount} wallet =
  let initVal = V.assetClassValue opAsset opAmount <> W.minAda
  in W.initialDistribution' [(wallet, [initVal])]
