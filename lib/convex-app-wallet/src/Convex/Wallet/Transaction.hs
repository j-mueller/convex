{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
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
  TxBodyState(..),
  TxBodyBalance(..),
  FinalTx(..),
  ModifiedBodyContent(..),
  PartialTx(..)
  ) where

import Cardano.Api (Address, AlonzoEra, BuildTx, Lovelace, NetworkId, ShelleyAddr, Tx, TxBody, TxBodyContent, TxId,
                    TxIn)
import Cardano.Api qualified as C
import Cardano.Api.Shelley (ProtocolParameters, StakePoolKey)
import Cardano.Api.Shelley qualified as C
import Cardano.Slotting.Time (SystemStart)
import Control.Lens (Lens', at, lens, makeLenses, over, use, view, (%=), (%~), (&), (.=), (.~), (<>~), (?=), (?~), (^.),
                     (|>))
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState, gets)
import Control.Monad.Writer (MonadWriter, tell)
import Convex.Wallet.Stats (Stats)
import Convex.Wallet.Stats qualified as Stats
import Convex.Wallet.Types (Wallet (..))
import Convex.Wallet.Types qualified as Types
import Convex.Wallet.Utils qualified as Utils
import Convex.Wallet.Utxos (UtxoState (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Either (rights)
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)

signTx :: Wallet -> TxBody AlonzoEra -> Tx AlonzoEra
signTx Wallet{wKey} body = C.signShelleyTransaction body [C.WitnessPaymentKey wKey]

data TxBodyState =
  Unbalanced -- ^ Tx body has not been balanced yet, potentially invalid txn
  | Balancing -- ^ Balancing in progress
  | AdaOnlyBalance -- ^ The balance contains only lovelace. Ada-only public key outputs can be used to fill it
  | Balanced -- ^ Tx body has been balanced and is valid (modulo key witnesses which are yet to be added)

data TxBodyBalance r where
  TxBodyBalanceUnbalanced :: TxBodyBalance Unbalanced -- ^ Balance is unknown
  TxBodyBalanceBalancing  :: C.Value -> TxBodyBalance Balancing -- ^ Balance is known
  TxBodyBalanceAdaOnly    :: Lovelace -> TxBodyBalance AdaOnlyBalance
  TxBodyBalanceBalanced   :: TxBodyBalance Balanced -- ^ Balance is zero, tx is balanced

data FinalTx r where
  FinalTxBalanced   :: C.BalancedTxBody AlonzoEra -> FinalTx Balanced
  FinalTxUnbalanced :: FinalTx Unbalanced
  FinalTxAdaOnly    :: FinalTx AdaOnlyBalance
  FinalTxBalancing  :: FinalTx Balancing

data ModifiedBodyContent r where
  ModBodyBalanced   :: ModifiedBodyContent Balanced
  ModBodyUnbalanced :: ModifiedBodyContent Unbalanced
  ModBodyBalancing  :: TxBodyContent BuildTx AlonzoEra -> ModifiedBodyContent Balancing
  ModBodyAdaOnly    :: TxBodyContent BuildTx AlonzoEra -> ModifiedBodyContent AdaOnlyBalance

data PartialTxLogEntry =
  AddPublicKeyOutput (Address ShelleyAddr) C.Value
  | AddPublicKeyInput TxIn
  | AddCollateral TxIn
  | AddSignature

data PartialTx r =
  PartialTx
    { ptxBodyContent    :: TxBodyContent BuildTx AlonzoEra -- ^ Original body content as submitted by client
    , ptxUtxo           :: Map TxIn Utils.TxOut -- ^ UTXOs spent by the transaction
    , ptxBodyContentMod :: ModifiedBodyContent r -- ^ Body content with modifications done by balancing algo
    , ptxBalance        :: TxBodyBalance r -- ^
    , ptxFinalTx        :: FinalTx r
    , ptxLog            :: Seq PartialTxLogEntry -- ^ Modifications that were made to the partial tranasction
    }

startBalancing :: PartialTx Unbalanced -> PartialTx Balancing
startBalancing PartialTx{ptxBodyContent, ptxUtxo} =
  PartialTx
    { ptxBodyContent
    , ptxUtxo
    , ptxBodyContentMod = ModBodyBalancing ptxBodyContent
    , ptxBalance        = TxBodyBalanceBalancing mempty
    , ptxFinalTx        = FinalTxBalancing
    }

utxoL :: Lens' (PartialTx r) (Map TxIn Utils.TxOut)
utxoL = lens ptxUtxo (\b u -> b{ptxUtxo = u})

logEntryL :: Lens' (PartialTx r) (Seq PartialTxLogEntry)
logEntryL = lens ptxLog (\b u -> b{ptxLog = u})

addLogEntry :: PartialTxLogEntry -> PartialTx r -> PartialTx r
addLogEntry e = over logEntryL (|> e)

utxos :: PartialTx r -> C.UTxO AlonzoEra
utxos = C.UTxO . Map.fromList . fmap (second (C.fromShelleyTxOut C.ShelleyBasedEraAlonzo)) . Map.toList . view utxoL

bodyContentL :: Lens' (PartialTx Balancing) (TxBodyContent BuildTx AlonzoEra)
bodyContentL =
  lens
    (\PartialTx{ptxBodyContentMod=ModBodyBalancing u} -> u)
    (\b u -> b{ptxBodyContentMod=ModBodyBalancing u})

bodyContentL' :: Lens' (PartialTx AdaOnlyBalance) (TxBodyContent BuildTx AlonzoEra)
bodyContentL' =
  lens
    (\PartialTx{ptxBodyContentMod=ModBodyAdaOnly u} -> u)
    (\b u -> b{ptxBodyContentMod=ModBodyAdaOnly u})

balanceL :: Lens' (PartialTx Balancing) C.Value
balanceL =
  lens
    (\PartialTx{ptxBalance=TxBodyBalanceBalancing u} -> u)
    (\b u -> b{ptxBalance=TxBodyBalanceBalancing u})

adaBalanceL :: Lens' (PartialTx AdaOnlyBalance) Lovelace
adaBalanceL =
  lens
    (\PartialTx{ptxBalance=TxBodyBalanceAdaOnly u} -> u)
    (\b u -> b{ptxBalance=TxBodyBalanceAdaOnly u})

toAdaOnlyBalance :: PartialTx Balancing -> Either FailureReason (PartialTx AdaOnlyBalance)
toAdaOnlyBalance ptx = do
  let balance = ptx ^. balanceL
      bd      = case ptxBodyContentMod ptx of { ModBodyBalancing b -> ModBodyAdaOnly b }

  bl <- case C.valueToList balance of
          []                  -> pure $ TxBodyBalanceAdaOnly 0
          [(C.AdaAssetId, x)] -> pure (TxBodyBalanceAdaOnly $ C.quantityToLovelace x)
          _                   -> Left UnexpectedNonAdaAmount
  pure ptx{ptxBodyContentMod = bd, ptxBalance = bl, ptxFinalTx = FinalTxAdaOnly}

addNonAdaChangeOutput :: Address ShelleyAddr -> PartialTx Balancing -> Either FailureReason (PartialTx AdaOnlyBalance)
addNonAdaChangeOutput addr ptx = do
  let balance = ptx ^. balanceL
      balWithoutAda = filter ((/=) C.AdaAssetId . fst) (C.valueToList balance)

  when (any ((0 < ) . snd) balWithoutAda)
    $ Left (NotSupported "negative non-Ada balance")
  let lvl = C.lovelaceToValue 2_500_000
      val = C.valueFromList balWithoutAda <> lvl
      k   = if (null balWithoutAda)
            then ptx
            else
              let ptx' = ptx & bodyContentL %~ Utils.addPublicKeyOutput' addr val
                             & addLogEntry (AddPublicKeyOutput addr val)
                             & balanceL <>~ C.negateValue lvl
              in ptx'
  toAdaOnlyBalance k

addAdaOnlyTxInput :: (TxIn, Utils.TxOut) -> PartialTx AdaOnlyBalance -> PartialTx AdaOnlyBalance
addAdaOnlyTxInput (txIn, txOut) ptx =
  let lvl = C.selectLovelace (Utils.txOutValue txOut) in
  ptx & bodyContentL' %~ (Utils.addKeyInput txIn txOut . Utils.addCollateral txIn)
      & adaBalanceL <>~ lvl
      & addLogEntry (AddPublicKeyInput txIn)
      & addLogEntry (AddCollateral txIn)
      & utxoL . at txIn ?~ txOut

addTxInputsToCoverBalance :: [(TxIn, Utils.TxOut)] -> PartialTx AdaOnlyBalance -> Either FailureReason (PartialTx AdaOnlyBalance)
addTxInputsToCoverBalance _ ptx@PartialTx{ptxBalance=TxBodyBalanceAdaOnly lovelace} | lovelace >= 0 = Right ptx
addTxInputsToCoverBalance (x:xs) ptx =
  let ptx' = addAdaOnlyTxInput x ptx
  in addTxInputsToCoverBalance xs ptx'
addTxInputsToCoverBalance [] PartialTx{ptxBalance=TxBodyBalanceAdaOnly lovelace} = Left (NotEnoughInputsForBalance lovelace)

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

balanceTxBody :: BalanceTxNodeEnv -> Wallet -> PartialTx AdaOnlyBalance -> Either FailureReason (PartialTx Balanced)
balanceTxBody e w ptx = do
  let utxo = utxos ptx
      bd   = view bodyContentL' ptx
      keyWitnesses = Just 1
      BalanceTxNodeEnv{bteEra, bteSystemStart, bteEraHistory, bteParams, bteActivePools, bteNetworkId} = e
      addr = C.shelleyAddressInEra (Types.address bteNetworkId w)
  case C.makeTransactionBodyAutoBalance bteEra bteSystemStart bteEraHistory bteParams bteActivePools utxo bd addr keyWitnesses of
    Left err -> Left (FailedToBalance err)
    Right bd ->
      Right $ ptx{ptxBodyContentMod = ModBodyBalanced, ptxBalance = TxBodyBalanceBalanced, ptxFinalTx = FinalTxBalanced bd}

type PoolId = C.Hash StakePoolKey

newtype NumRetries = NumRetries{ getNumRetries :: Int }
  deriving stock (Eq, Ord, Show)

data FailureReason =
  FailedToBalance C.TxBodyErrorAutoBalance -- ^ 'C.makeTransactionBodyAutoBalance' failed. TODO: We can recover from some of these errors, for example TxBodyErrorValidityInterval
  | PolicyViolation
  | UnexpectedNonAdaAmount
  | NotEnoughInputsForBalance Lovelace
  | NotSupported String -- ^ Failed to balance because of a feature that is currently unsupported
  | InputSpentByDifferentTx TxIn TxId -- ^ One of the transaction's inputs was spent by a different transaction
  deriving (Show)

data TransactionStatus =
  WaitingForUtxo NumRetries
  | Failed FailureReason -- ^ Txn failed and will not be retried
  | Submitted TxId
  | Accepted TxId
  deriving stock (Show)

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
    , _txnsToBeSubmitted  :: Seq (PartialTx Balanced) -- TODO: 1. Tx type. 2. should probably use an MVar?
    , _txnStatus          :: !(Map TxRequestId TransactionStatus) -- ^ The status of every request that has been received.
    , _queuedTransactions :: !(Seq (TxRequestId, PartialTx Unbalanced)) -- ^ Transactions that are waiting for UTXOs to become available.
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
enqueueTx :: (MonadWriter Stats m, MonadState TxQueueState m) => TxRequestId -> PartialTx Unbalanced -> m ()
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
  | BalancedTx (PartialTx Balanced) -- ^ Transaction was dequeued and balanced

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
balanceTx wallet@Wallet{wNonAdaReturnAddress} utxoState BalanceTxNodeEnv{bteParams, bteNetworkId, bteActivePools} = gets (Seq.viewl . _queuedTransactions) >>= \case
  (requestId, exportTx) Seq.:< rest -> do
    utxos_ <- availableUtxos utxoState
    case utxos_ of
      [] -> pure NotEnoughUTXOs
      (x:xs) -> do
        let wipTx = startBalancing exportTx
            txWithProtocolParams = over bodyContentL (Utils.setProtocolParams bteParams) wipTx
            walletAddress        = Types.address bteNetworkId wallet

            -- cand0 is preferred over cand1
            cand0 =
              let vl = C.lovelaceToValue 3_500_000
              in txWithProtocolParams & bodyContentL %~ Utils.addPublicKeyOutput' walletAddress vl & addLogEntry (AddPublicKeyOutput walletAddress vl)
            cand1 = txWithProtocolParams

            -- partial transactions with balance
            candidatesWithBal :: [PartialTx Balancing]
            candidatesWithBal =
              let mkCandidate  =
                    -- add dummy collateral and tx out to prevent certain 'TxBodyError's
                    C.makeTransactionBody . Utils.addPublicKeyOutput walletAddress 0 . Utils.addCollateral (fst x) . view bodyContentL
                  compBal partialTx body =
                    let bal = fmap (C.evaluateTransactionBalance bteParams bteActivePools (utxos wipTx)) body
                    in fmap (\b -> partialTx & balanceL .~ Utils.txOutValue' b) bal
              in rights $ fmap (\cand -> compBal cand $ mkCandidate cand) [cand0, cand1]

            candidatesWithNonAdaChangeAdded = rights $ addNonAdaChangeOutput wNonAdaReturnAddress <$> candidatesWithBal

            -- now select ada inputs until the balance is positive
            candidatesWithInputs = rights $ addTxInputsToCoverBalance (x:xs) <$> candidatesWithNonAdaChangeAdded


        -- try to balance the candidates until we find one that works
        undefined
  Seq.EmptyL -> pure EmptyQueue
