{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

{-| Utilities for convex-app-wallet
-}
module Convex.Wallet.Utils(spentTxIns) where

import Cardano.Api (AlonzoEra, Block (..), BlockInMode (..), CardanoMode, EraInMode (AlonzoEraInCardanoMode),
                    Tx (..), TxId, TxIn (..), TxIx (..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley (TxBody (..))
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo.TxBody
import Cardano.Ledger.TxIn qualified as CT
import Data.Bifunctor (Bifunctor (..))
import Data.Set qualified as Set

spentTxIns :: BlockInMode CardanoMode -> [(TxIn, TxId)]
spentTxIns = \case
  BlockInMode block AlonzoEraInCardanoMode -> spentTxInsAlonzoBlock block
  _                                        -> []

spentTxInsAlonzoBlock :: Block AlonzoEra -> [(TxIn, TxId)]
spentTxInsAlonzoBlock (Block _blockHeader txns) =
  foldMap txIns txns

txIns :: Tx AlonzoEra -> [(TxIn, TxId)]
txIns (Tx txBody _) =
  let txId = C.getTxId txBody
      ShelleyTxBody _ txBody' _scripts _scriptData _auxiliaryData _ = txBody
      Alonzo.TxBody.TxBody{Alonzo.TxBody.inputs} = txBody'
  in fmap ((,txId) . uncurry TxIn . bimap CS.fromShelleyTxId (TxIx . fromIntegral) . (\(CT.TxIn i n) -> (i, n)))
        $ Set.toList inputs
