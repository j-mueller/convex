{-# LANGUAGE NamedFieldPuns #-}
{-| Some off-chain code for creating and exercising options
-}
module Convex.Options.OffChain(
  initialise,
  exercise,
  reclaim
  ) where

import Convex.Options.OnChain.Scripts as Scripts
import Convex.Options.OnChain.Types (MPSRedeemer (..), Option (..), OptionInstance (..), OptionRedeemer (..),
                                     OptionState (..), exerciseInterval, initialParam, optionValueLocked,
                                     reclaimInterval)
import Cooked.MockChain.Monad (MonadBlockChain (..), pkUtxos, scriptUtxosSuchThat, validateTxConstrLbl)
import Cooked.Tx.Constraints qualified as C
import Ledger (AssetClass, Value)
import Ledger.Value qualified as Value

data OptionLbl = InitialiseTx | ExerciseTx | ReclaimTx
  deriving Show

buyerSellerTokens :: OptionInstance -> Value
buyerSellerTokens OptionInstance{oppOption=Option{opBuyerTN, opSellerTN}, oppCurrency} =
  Value.singleton oppCurrency opBuyerTN 1
    <> Value.singleton oppCurrency opSellerTN 1

utxoAssetClass :: OptionInstance -> AssetClass
utxoAssetClass OptionInstance{oppOption=Option{opUtxoTN}, oppCurrency} =
  Value.assetClass oppCurrency opUtxoTN

mintValue :: OptionInstance -> Value
mintValue inst =
  buyerSellerTokens inst
    <> Value.assetClassValue (utxoAssetClass inst) 1

{-| Create an options contract, paying the tokens to the
caller.
-}
initialise :: MonadBlockChain m => Option -> m OptionInstance
initialise option = do
  pkh <- ownPaymentPubKeyHash
  oref:_ <- pkUtxos pkh
  let (utxo, _) = oref
      param = initialParam (Scripts.mintingPolicySymbol option utxo) option
      vl = optionValueLocked param
      constraints =
        [ C.Mints (Just MPSMinting) [Scripts.mintingPolicy option utxo] (mintValue param)
        , C.PaysScript Scripts.typedValidator [(param, vl)]
        , C.SpendsPK oref
        , C.SignedBy [pkh]
        , C.PaysPK pkh (buyerSellerTokens param)
        ]
  _ <- validateTxConstrLbl InitialiseTx constraints
  pure param

exercise :: MonadBlockChain m => OptionInstance -> m OptionInstance
exercise inst@OptionInstance{oppOption=option@Option{opUtxoTN}, oppCurrency} = do
  pkh <- ownPaymentPubKeyHash
  let ac = Value.assetClass oppCurrency opUtxoTN
  utxos <- scriptUtxosSuchThat Scripts.typedValidator (\_ vl -> Value.assetClassValueOf vl ac == 1)
  case utxos of
    [(spendableOut, _)] -> do
      let newState = inst{oppState = Exercised}
          newVal   = optionValueLocked newState
          constraints =
            [ C.PaysScript Scripts.typedValidator [(newState, newVal)]
            , C.SpendsScript Scripts.typedValidator Exercise (spendableOut, inst)
            , C.PaysPK pkh (buyerSellerTokens inst)
            , C.ValidateIn (exerciseInterval option)
            ]
      _ <- validateTxConstrLbl ExerciseTx constraints
      pure newState
    _ -> fail "Failed to find UTXO"

reclaim :: MonadBlockChain m => OptionInstance -> m ()
reclaim inst@OptionInstance{oppOption=option@Option{opUtxoTN}, oppCurrency} = do
  pkh <- ownPaymentPubKeyHash
  let ac = Value.assetClass oppCurrency opUtxoTN
  utxos <- scriptUtxosSuchThat Scripts.typedValidator (\_ vl -> Value.assetClassValueOf vl ac == 1)
  case utxos of
    [(spendableOut, _)] -> do
      let constraints =
            [ C.SpendsScript Scripts.typedValidator Reclaim (spendableOut, inst)
            , C.PaysPK pkh (buyerSellerTokens inst)
            , C.ValidateIn (reclaimInterval option)
            ]
      _ <- validateTxConstrLbl ReclaimTx constraints
      pure ()
    _ -> fail "Failed to find UTXO"
