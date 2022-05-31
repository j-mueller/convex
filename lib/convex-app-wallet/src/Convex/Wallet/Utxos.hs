{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
module Convex.Wallet.Utxos(
  -- * UTXOs of the wallet
  UtxoState(..),
  _UtxoState,
  -- * Changes to the wallet's UTXO set
  UtxoChange(..),
  utxosAdded,
  utxosRemoved,
  inv,
  apply,
  -- ** Get changes from alonzo blocks
  extract
  ) where

import Cardano.Api (AlonzoEra, Block (..), BlockInMode (..), CardanoMode, EraInMode (AlonzoEraInCardanoMode),
                    PaymentCredential, Tx (..), TxIn (..), TxIx (..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley (TxBody (..))
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.Address qualified as Address
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo.TxBody
import Cardano.Ledger.TxIn qualified as CT
import Control.Lens (makeLenses, makePrisms)
import Convex.Wallet.Utils (TxOut)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

{-| The UTXOs currently available to the wallet
-}
newtype UtxoState =
    UtxoState
      { _walletUtxos      :: Map TxIn TxOut -- ^ Map of all UTXOs that are currently available to the wallet. This map is read from and written to by the node client.
      } deriving stock (Eq, Show)
        deriving newtype (Semigroup, Monoid)

makePrisms ''UtxoState

{-| Change to the UTXO set. This forms a change action on the UTXO state with 'apply'.
-}
data UtxoChange =
  UtxoChange
    { _utxosAdded   :: !(Map TxIn TxOut)
    , _utxosRemoved :: !(Map TxIn TxOut) -- TODO: How can we keep track of the reason for removal? Spent vs rolled back
    }

makeLenses ''UtxoChange

instance Semigroup UtxoChange where
  l <> r =
    let bothAdded   = _utxosAdded l <> _utxosAdded r
        bothRemoved = _utxosRemoved l <> _utxosRemoved r
    in UtxoChange
        { _utxosAdded = bothAdded
        , _utxosRemoved = bothRemoved
        }

instance Monoid UtxoChange where
  mempty = UtxoChange mempty mempty

{-| Invert a 'UtxoChange' value such that @x <> invert x == mempty@ and @invert x <> x == mempty@
-}
inv :: UtxoChange -> UtxoChange
inv (UtxoChange added removed) = UtxoChange removed added

{-| Change the 'UtxoState'
-}
apply :: UtxoState -> UtxoChange -> UtxoState
apply UtxoState{_walletUtxos} UtxoChange{_utxosAdded, _utxosRemoved} =
  UtxoState $ (_walletUtxos `Map.union` _utxosAdded) `Map.difference` _utxosRemoved

{-| Extract the 'UtxoChange' from an alonzo-era block.
-}
extract :: PaymentCredential -> UtxoState -> BlockInMode CardanoMode -> UtxoChange
extract credential utxos = \case
  BlockInMode block AlonzoEraInCardanoMode -> extractAlonzoBlock credential utxos block
  _                                        -> mempty

extractAlonzoBlock :: PaymentCredential -> UtxoState -> Block AlonzoEra -> UtxoChange
extractAlonzoBlock credential utxos (Block _blockHeader txns) =
  foldMap (changeFromTxn credential utxos) txns

changeFromTxn :: PaymentCredential -> UtxoState -> Tx AlonzoEra -> UtxoChange
changeFromTxn credential UtxoState{_walletUtxos} (Tx txBody _) =
  let txId = C.getTxId txBody
      ShelleyTxBody _ txBody' _scripts _scriptData _auxiliaryData _ = txBody
      Alonzo.TxBody.TxBody{Alonzo.TxBody.outputs, Alonzo.TxBody.inputs} = txBody'

      checkOutput :: TxIx -> TxOut -> Maybe (TxIn, TxOut)
      checkOutput txIx txOut = case txOut of
        (Alonzo.TxBody.TxOut (Address.Addr _network paymentCredential _stakeReference) _ _) | CS.fromShelleyPaymentCredential paymentCredential == credential -> Just (TxIn txId txIx, txOut)
        _                               -> Nothing

      checkInput :: TxIn -> Maybe (TxIn, TxOut)
      checkInput txIn = fmap (txIn,) $ Map.lookup txIn _walletUtxos

      _utxosAdded = Map.fromList $ mapMaybe (uncurry checkOutput) (zip (TxIx <$> [0..]) (toList outputs))
      _utxosRemoved = Map.fromList
        $ mapMaybe checkInput
        $ fmap (uncurry TxIn . bimap CS.fromShelleyTxId (TxIx . fromIntegral) . (\(CT.TxIn i n) -> (i, n)))
        $ Set.toList inputs
  in UtxoChange{_utxosAdded, _utxosRemoved}

