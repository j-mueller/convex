{-# LANGUAGE NamedFieldPuns #-}
{-| Some off-chain code for creating and exercising options
-}
module Convex.Options.OffChain(
  initialiseOption
  ) where

import Convex.Options.OnChain.Scripts as Scripts
import Convex.Options.OnChain.Types (MPSRedeemer (..), Option (..), OptionParam (..), initialParam, optionValueLocked)
import Cooked.MockChain.Monad (MonadBlockChain (..), pkUtxos, validateTxConstrLbl)
import Cooked.Tx.Constraints qualified as C
import Ledger (TxOutRef, Value)
import Ledger.Value qualified as Value

data OptionLbl = InitialiseTx | ExerciseTx | ReclaimTx
  deriving Show

mintValue :: Option -> TxOutRef -> Value
mintValue option@Option{opBuyerTN, opSellerTN} utxo =
  let mps = Scripts.mintingPolicySymbol option utxo
  in Value.singleton mps opBuyerTN 1
      <> Value.singleton mps opSellerTN 1

{-| Create an options contract, paying the tokens to the
caller.
-}
initialiseOption :: MonadBlockChain m => Option -> m ()
initialiseOption option = do
  pkh <- ownPaymentPubKeyHash
  oref:_ <- pkUtxos pkh
  let mv = mintValue option (fst oref)
      param = initialParam (Scripts.mintingPolicySymbol option (fst oref)) option
      vl = optionValueLocked (oppState param) option
      constraints =
        [ C.Mints (Just MPSMinting) [Scripts.mintingPolicy option (fst oref)] mv
        , C.PaysScript Scripts.typedValidator [(param, vl)]
        , C.SpendsPK oref
        , C.SignedBy [pkh]
        , C.PaysPK pkh mv
        ]
  _ <- validateTxConstrLbl InitialiseTx constraints
  pure ()

