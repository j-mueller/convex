{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
{-| Types for transactions that are dealt with by the wallet
-}
module Convex.Wallet.Transaction(
  -- * Signing transactions
  signTx,
  -- * Tx processor state
  TxRequestId(..),
  TxProcessorState(..),
  maxRequestId,
  nextId,
  -- * Transaction status
  TransactionStatus(..),
  TxQueueState(..),
  BalanceTxResult(..),
  queuedTransactions,
  txnsToBeSubmitted,
  enqueueTx,
  emptyTxQueueState,
  processSpentTxIn,
  processTxInSpentEvent,
  balanceTx,
  -- * Tx state
  TxBodyInState(..),
  TxBodyState(..)
  ) where

import Cardano.Api (AlonzoEra, BuildTx, NetworkId, Tx, TxBody, TxBodyContent, TxId, TxIn)
import Cardano.Api qualified as C
import Cardano.Api.Shelley (ProtocolParameters, StakePoolKey)
import Cardano.Slotting.Time (SystemStart)
import Control.Lens (at, makeLenses, use, view, (%=), (.=), (?=), (|>))
import Control.Monad.State (MonadState, gets)
import Control.Monad.Writer (MonadWriter, tell)
import Convex.Wallet.Stats (Stats)
import Convex.Wallet.Stats qualified as Stats
import Convex.Wallet.Types (Wallet (..))
import Convex.Wallet.Types qualified as Types
import Convex.Wallet.Utils qualified as Utils
import Convex.Wallet.Utxos (UtxoState (..))
import Data.List (inits, sortOn)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)

signTx :: Wallet -> TxBody AlonzoEra -> Tx AlonzoEra
signTx Wallet{wKey} body = C.signShelleyTransaction body [C.WitnessPaymentKey wKey]

data TxBodyState =
  Unbalanced -- ^ Tx body has not been balanced yet, potentially invalid txn
  | Balanced -- ^ Tx body has been balanced and is valid (modulo key witnesses which are yet to be added)

newtype TxBodyInState (state :: TxBodyState) = TxBodyInState { unTxBodyInState :: TxBody AlonzoEra }

type PoolId = C.Hash StakePoolKey

newtype NumRetries = NumRetries{ getNumRetries :: Int }
  deriving stock (Eq, Ord, Show)

data FailureReason =
  FailedToBalance
  | PolicyViolation
  | InputSpentByDifferentTx TxIn TxId -- ^ One of the transaction's inputs was spent by a different transaction
  deriving (Eq, Ord, Show)

data TransactionStatus =
  WaitingForUtxo NumRetries
  | Failed FailureReason -- ^ Txn failed and will not be retried
  | Submitted TxId
  | Accepted TxId
  deriving stock (Eq, Ord, Show)

newtype TxRequestId = TxRequestId Integer
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Enum)

data TxProcessorState =
  TxProcessorState
    { _maxRequestId       :: !TxRequestId -- ^ Highest request ID that has been assigned so far.
    }

makeLenses ''TxProcessorState

data TxQueueState =
  TxQueueState
    { _pendingUtxos       :: !(Map TxIn TxRequestId) -- ^ UTXOs that are used by transactions that have not appeared on the chain yet.
    , _txnsToBeSubmitted  :: Seq (TxBodyInState 'Balanced) -- TODO: 1. Tx type. 2. should probably use an MVar?
    , _txnStatus          :: !(Map TxRequestId TransactionStatus) -- ^ The status of every request that has been received.
    , _queuedTransactions :: !(Seq (TxRequestId, TxBodyContent BuildTx AlonzoEra)) -- ^ Transactions that are waiting for UTXOs to become available.
    }

makeLenses ''TxQueueState

emptyTxQueueState :: TxQueueState
emptyTxQueueState = TxQueueState mempty mempty mempty mempty

{-| Get a fresh request ID. This will be called by the webserver.
-}
nextId :: MonadState TxProcessorState m => m TxRequestId
nextId = do
  maxRequestId %= succ
  use maxRequestId

{-| Add a transaction to the queue
-}
enqueueTx :: (MonadWriter Stats m, MonadState TxQueueState m) => TxRequestId -> TxBodyContent BuildTx AlonzoEra -> m ()
enqueueTx rqId etx = do
  txnStatus . at rqId ?= (WaitingForUtxo $ NumRetries 0)
  queuedTransactions %= (|> (rqId, etx))
  tell Stats.txnReceived -- ^ TODO: We should count this in the webserver when the tx first enters the system. Otherwise we might end up counting the transaction several times, if it gets dequeued and then enqueued again?
  -- TODO: Also add to pendingUtxos

data TxInSpentEvent = TxInSpentEvent{ tseTxIn :: TxIn, tseRequestId :: TxRequestId, tseSpendingTx :: TxId }

{-| Process a spent transaction input
-}
processSpentTxIn :: MonadState TxQueueState m => TxIn -> TxId -> m (Maybe TxInSpentEvent)
processSpentTxIn tseTxIn tseSpendingTx = do
  use (pendingUtxos . at tseTxIn) >>= \case
    Just tseRequestId -> do
      pendingUtxos . at tseTxIn .= Nothing
      pure (Just TxInSpentEvent{tseTxIn, tseRequestId, tseSpendingTx})
    Nothing -> pure Nothing

-- balance tx & submit
-- retry :: RequestId -> m ()
-- cancel :: RequestId -> m ()

processTxInSpentEvent :: MonadState TxQueueState m => TxInSpentEvent -> m ()
processTxInSpentEvent TxInSpentEvent{tseTxIn, tseRequestId, tseSpendingTx} = do
  use (txnStatus . at tseRequestId) >>= \case
    Just (Submitted txId) | txId == tseSpendingTx -> txnStatus . at tseRequestId ?= Accepted txId
    _ -> txnStatus . at tseRequestId ?= Failed (InputSpentByDifferentTx tseTxIn tseSpendingTx)

{-| Outcome of calling 'balanceTx'
-}
data BalanceTxResult =
  EmptyQueue -- ^ No transactions in queue
  | NotEnoughUTXOs -- ^ Not enough UTXOs to balance the transaction
  | BalancedTx (TxBodyInState 'Balanced) -- ^ Transaction was dequeued and balanced

{-| Network information (available from cardano node)
-- TODO: NodeEnv -> IO BalanceTxNodeEnv
-}
data BalanceTxNodeEnv =
  BalanceTxNodeEnv
    { bteEra         :: C.EraInMode C.AlonzoEra C.CardanoMode
    , bteSystemStart :: SystemStart
    , bteEraHistory  :: C.EraHistory C.CardanoMode
    , bteParams      :: ProtocolParameters
    , bteActivePools :: Set PoolId
    , bteNetworkId   :: NetworkId
    }

{-| Return the UTXOs that can be used for balancing a transaction.
The UTXOs are
* Ada only
* Not earmarked for use by any other transaction
* Sorted by Lovelace value in ascending order
-}
availableUtxos :: MonadState TxQueueState m => UtxoState -> m [(TxIn, Utils.TxOut)]
availableUtxos UtxoState{_walletUtxos} = do
  pending <- use pendingUtxos
  return
    $ sortOn (C.selectLovelace . Utils.txOutValue . snd)
    $ Map.toList
    $ Map.filter (not . Utils.hasNonAdaAssets . Utils.txOutValue)
    $ Map.difference _walletUtxos pending

{-| Dequeue the first transaction and try to balance it.
-}
balanceTx :: MonadState TxQueueState m => Wallet -> UtxoState -> BalanceTxNodeEnv -> m BalanceTxResult
balanceTx wallet utxoState BalanceTxNodeEnv{bteParams, bteNetworkId} = gets (Seq.viewl . _queuedTransactions) >>= \case
  (requestId, exportTx) Seq.:< rest -> do
    utxos <- availableUtxos utxoState
    case utxos of
      [] -> pure NotEnoughUTXOs
      xs -> do
        let txWithProtocolParams = exportTx{C.txProtocolParams = C.BuildTxWith $ Just bteParams }

            -- cand0 is preferred over cand1
            cand0 = Utils.addPublicKeyOutput txWithProtocolParams (Types.address bteNetworkId wallet) 3_500_000
            cand1 = txWithProtocolParams

        -- set collateral
        -- non-ada change

        -- try to balance the candidates until we find one that works
        undefined
    -- TODO:
    -- 1. Compute balance
    --       2. if there is a positive non-ada balance, add spare change outputs (to ensure collateral preservation)
    --       3. compute balance again
    --       4. add available UTXOs until balance is met
    --       5. call makeTransactionBodyAutoBalance https://input-output-hk.github.io/cardano-node/cardano-api/Cardano-Api.html#v:makeTransactionBodyAutoBalance
  Seq.EmptyL -> pure EmptyQueue
