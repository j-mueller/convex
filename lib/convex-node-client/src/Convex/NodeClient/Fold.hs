{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-| A node client that applies a fold to the stream of blocks.
Unlike 'foldBlocks' from 'Cardano.Api', this one supports rollbacks.
-}
module Convex.NodeClient.Fold(
  CatchingUp(..),
  foldClient
  ) where

import Cardano.Api (Block (..), BlockHeader (..), BlockInMode (..), BlockNo (..), CardanoMode, ChainPoint (..),
                    ChainTip (..), Env, SlotNo, envSecurityParam)
import Cardano.Slotting.Slot (WithOrigin (At))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Convex.NodeClient.Types (ClientBlock, PipelinedLedgerStateClient (..), fromChainTip)
import Network.TypedProtocol.Pipelined (Nat (..))
import Ouroboros.Consensus.Block.Abstract (WithOrigin (..))
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined (ClientPipelinedStIdle (..), ClientStNext (..))
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (PipelineDecision (..), pipelineDecisionMax)

{-| Whether we have fully caught up with the node
-}
data CatchingUp =
  CatchingUpWithNode -- ^ Client is still catching up
  | CaughtUpWithNode -- ^ Client fully caught up (client tip == server tip)
  deriving (Eq, Ord, Show)

{-| Run the client until 'Nothing' is returned
-}
foldClient ::
  forall s.
  s -> -- ^ Initial state
  Env -> -- ^ Node connection data
  (CatchingUp -> s -> BlockInMode CardanoMode -> IO (Maybe s)) -> -- ^ Fold
  PipelinedLedgerStateClient
foldClient initialState env applyBlock = PipelinedLedgerStateClient $ CSP.ChainSyncClientPipelined $ do

-- NB: The code below was adapted from https://input-output-hk.github.io/cardano-node/cardano-api/src/Cardano.Api.LedgerState.html#foldBlocks

  let
    pipelineSize = 50 -- TODO: Configurable

    initialHistory = initialStateHistory initialState

    clientIdle_RequestMoreN
      :: forall n. WithOrigin BlockNo
      -> WithOrigin BlockNo
      -> Nat n -- Number of requests inflight.
      -> History s
      -> CSP.ClientPipelinedStIdle n ClientBlock ChainPoint ChainTip IO ()
    clientIdle_RequestMoreN clientTip serverTip n history
      = case pipelineDecisionMax pipelineSize n clientTip serverTip  of
          Collect -> case n of
            Succ predN -> CSP.CollectResponse Nothing (clientNextN predN history)
          _ -> CSP.SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip serverTip (Succ n) history)

    clientNextN
      :: Nat n
      -> History s
      -> ClientStNext n ClientBlock ChainPoint ChainTip IO ()
    clientNextN n history =
      ClientStNext {
          recvMsgRollForward = \newBlock serverChainTip -> do
              let BlockInMode (Block (BlockHeader slotNo _blockHash currBlockNo) _) _ = newBlock
                  newClientTip = At currBlockNo
                  newServerTip = fromChainTip serverChainTip
                  cu = if newClientTip == newServerTip then CaughtUpWithNode else CatchingUpWithNode
                  currentState =
                    case Seq.viewl history of
                      (_, x) Seq.:< _ -> x
                      Seq.EmptyL      -> error "foldClient: clientNextN: Impossible - empty history!"

              newState <- applyBlock cu currentState newBlock
              case newState of
                Nothing -> do
                  putStrLn "foldClient: Shutting down"
                  clientIdle_DoneN n
                Just !s' -> do
                  let (newHistory, _) = pushHistoryState env history slotNo s'
                  return (clientIdle_RequestMoreN newClientTip newServerTip n newHistory)
        , recvMsgRollBackward = \chainPoint serverChainTip -> do
            putStrLn $ "foldClient: Rolling back to" <> show chainPoint
            let newClientTip = Origin
                newServerTip = fromChainTip serverChainTip
                truncatedHistory = case chainPoint of
                    ChainPointAtGenesis -> initialHistory
                    ChainPoint slotNo _ -> rollbackStateHistory history slotNo
            return (clientIdle_RequestMoreN newClientTip newServerTip n truncatedHistory)
        }

    clientIdle_DoneN
      :: Nat n
      -> IO (ClientPipelinedStIdle n ClientBlock ChainPoint ChainTip IO ())
    clientIdle_DoneN n = case n of
      Succ predN -> do
        return $ CollectResponse Nothing (clientNext_DoneN predN) -- Ignore remaining message responses
      Zero -> do
        putStrLn "Chain Sync: done!"
        return $ SendMsgDone ()

    clientNext_DoneN
      :: Nat n
      -> ClientStNext n ClientBlock ChainPoint ChainTip IO ()
    clientNext_DoneN n =
      ClientStNext {
          recvMsgRollForward = \_ _ -> clientIdle_DoneN n
        , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n
        }

  return (clientIdle_RequestMoreN Origin Origin Zero initialHistory)

-- | A history of the last @k@ states
type History a = Seq (SlotNo, a)

-- | Add a new state to the history
pushHistoryState
  :: Env                -- ^ Environement used to get the security param, k.
  -> History a          -- ^ History of k items.
  -> SlotNo             -- ^ Slot number of the new item.
  -> a                  -- ^ New item to add to the history
  -> (History a, History a)
  -- ^ ( The new history with the new item appended
  --   , Any exisiting items that are now past the security parameter
  --      and hence can no longer be rolled back.
  --   )
pushHistoryState env hist ix st
  = Seq.splitAt
      (fromIntegral $ envSecurityParam env + 1)
      ((ix, st) Seq.:<| hist)

rollbackStateHistory :: History a -> SlotNo -> History a
rollbackStateHistory hist maxInc = Seq.dropWhileL ((> maxInc) . (\(x,_) -> x)) hist

initialStateHistory :: a -> History a
initialStateHistory a = Seq.singleton (0, a)
