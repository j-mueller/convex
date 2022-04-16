{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
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
  queuedTransactions,
  txnsToBeSubmitted,
  enqueueTx,
  emptyTxQueueState,
  processSpentTxIn,
  processTxInSpentEvent,
  -- * Tx state
  TxBodyInState(..),
  TxBodyState(..)
  ) where

import Cardano.Api (AlonzoEra, Tx, TxBody, TxId, TxIn)
import Cardano.Api qualified as C
import Control.Lens (at, makeLenses, use, (%=), (.=), (?=), (|>))
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, tell)
import Convex.Wallet.ExportTx (ExportTx)
import Convex.Wallet.Stats (Stats)
import Convex.Wallet.Stats qualified as Stats
import Convex.Wallet.Types (Wallet (..))
import Data.Map.Strict (Map)
import Data.Sequence (Seq)

signTx :: Wallet -> TxBody AlonzoEra -> Tx AlonzoEra
signTx Wallet{wKey} body = C.signShelleyTransaction body [C.WitnessPaymentKey wKey]

data TxBodyState =
  Unbalanced -- ^ Tx body has not been balanced yet, potentially invalid txn
  | Balanced -- ^ Tx body has been balanced and is valid (modulo key witnesses which are yet to be added)

newtype TxBodyInState (state :: TxBodyState) = TxBodyInState { unTxBodyInState :: TxBody AlonzoEra }

-- makeTransactionBodyAutoBalance

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
    , _queuedTransactions :: !(Seq (TxRequestId, ExportTx)) -- ^ Transactions that are waiting for UTXOs to become available.
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
enqueueTx :: (MonadWriter Stats m, MonadState TxQueueState m) => TxRequestId -> ExportTx -> m ()
enqueueTx rqId etx = do
  -- TODO: Write stats
  txnStatus . at rqId ?= (WaitingForUtxo $ NumRetries 0)
  queuedTransactions %= (|> (rqId, etx))
  tell Stats.txnReceived
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
