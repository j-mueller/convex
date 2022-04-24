{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-| Node client of the convex-app-wallet
-}
module Convex.Wallet.NodeClient(
  WalletState(..),
  emptyWalletState,
  Delta,
  walletClient
  ) where

import Cardano.Api (AlonzoEra, Block (..), BlockInMode (..), BuildTx, CardanoMode, Env, TxBodyContent)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.State.Strict (execStateT)
import Control.Monad.Writer (MonadWriter, runWriterT)
import Convex.NodeClient (PipelinedLedgerStateClient, foldClient')
import Convex.Wallet.Stats (Stats, WalletStats)
import Convex.Wallet.Stats qualified as Stats
import Convex.Wallet.Transaction (PartialTx, TxBodyState (..), TxQueueState, TxRequestId, emptyTxQueueState, enqueueTx,
                                  processSpentTxIn, processTxInSpentEvent)
import Convex.Wallet.Types (Wallet)
import Convex.Wallet.Types qualified as Wallet
import Convex.Wallet.Utils qualified as U
import Convex.Wallet.Utxos (UtxoState (..))
import Convex.Wallet.Utxos qualified as Utxos
import Data.Foldable (traverse_)
import Data.Sequence (Seq)

type Delta = Seq (TxRequestId, PartialTx Unbalanced)

data WalletState =
  WalletState
    { _queueState :: TxQueueState
    , _utxoState  :: UtxoState
    }

emptyWalletState :: WalletState
emptyWalletState = WalletState emptyTxQueueState mempty

walletClient ::
  Env
  -> Wallet
  -> TVar WalletStats
  -> TVar Delta
  -> TVar UtxoState
  -> PipelinedLedgerStateClient
walletClient env wallet statsT deltaT _tv = foldClient' emptyWalletState env rollbackWallet' rollforwardWallet' where

  rollbackWallet' _ delta oldState = do
    -- When rolling back we put the requests back into the queue
    STM.atomically (STM.modifyTVar deltaT (delta <>))
    pure (mempty, oldState)

  rollforwardWallet' _catchingUp WalletState{_utxoState, _queueState} newBlock = do
    let BlockInMode (Block blockHeader _) _ = newBlock
    -- Compute the UTXOs that are available to the wallet
    let updatedState = Utxos.apply _utxoState (Utxos.extract (Wallet.paymentCredential wallet) _utxoState newBlock)

    -- When rolling forward we remove requests from the queue and process them.
    delta <- STM.atomically (STM.swapTVar deltaT mempty)

    --
    runMaybeT $ do
      (newQueueState, stats) <- runWriterT $ flip execStateT _queueState $ do
        rollforwardWallet updatedState delta newBlock
      liftIO $ STM.atomically $ STM.modifyTVar' statsT (Stats.prepend (Stats.fromBlockHeader blockHeader stats))
      return (delta, WalletState{_utxoState = updatedState, _queueState = newQueueState})

rollforwardWallet :: (MonadState TxQueueState m, MonadWriter Stats m) => UtxoState -> Delta -> BlockInMode CardanoMode -> m ()
rollforwardWallet UtxoState{} delta block = do

  -- add new user submitted transactions
  traverse_ (uncurry enqueueTx) delta

  -- process all inputs that were spent by the current block
  traverse_ (uncurry processSpentTxIn >=> traverse_ processTxInSpentEvent) (U.spentTxIns block)

  -- TODO: Select txn to be processed (if enough tx Ins are available)
  pure ()

