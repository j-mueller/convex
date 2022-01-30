{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
module Convex.NodeClient.Types(
  PipelinedLedgerStateClient(..),
  ClientBlock,
  runNodeClient,
  -- * Sync points
  ChainPoint(..),
  fromChainTip
  ) where

import Cardano.Api (BlockInMode (..), BlockNo (..), CardanoMode, ChainPoint (..), ChainSyncClientPipelined,
                    ChainTip (..), ConsensusModeParams (..), Env (..), EpochSlots (..), InitialLedgerStateError,
                    LocalChainSyncClient (LocalChainSyncClientPipelined), LocalNodeClientProtocols (..),
                    LocalNodeClientProtocolsInMode, LocalNodeConnectInfo (..), NetworkId (Mainnet, Testnet),
                    NetworkMagic (..), connectToLocalNode, envSecurityParam)
import Cardano.Api qualified as CAPI
import Cardano.Chain.Genesis qualified
import Cardano.Crypto (ProtocolMagicId (unProtocolMagicId), RequiresNetworkMagic (..))
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Data.SOP.Strict (NP ((:*)))
import Ouroboros.Consensus.Cardano.CanHardFork qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras qualified as HFC
import Ouroboros.Consensus.HardFork.Combinator.Basics qualified as HFC
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP

{-|
-}
newtype PipelinedLedgerStateClient =
  PipelinedLedgerStateClient
    { getPipelinedLedgerStateClient :: ChainSyncClientPipelined (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    }

runNodeClient ::
  FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> FilePath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> (LocalNodeConnectInfo CardanoMode -> Env -> IO PipelinedLedgerStateClient)
  -- ^ Client
  -> ExceptT InitialLedgerStateError IO ()
  -- ^ The final state
runNodeClient nodeConfigFilePath socketPath client = do
  (env, _ledgerState) <- withExceptT id (CAPI.initialLedgerState nodeConfigFilePath)

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig
        = (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
        . HFC.getPerEraLedgerConfig
        . HFC.hardForkLedgerConfigPerEra
        $ envLedgerConfig env

      networkMagic
        = NetworkMagic
        $ unProtocolMagicId
        $ Cardano.Chain.Genesis.gdProtocolMagicId
        $ Cardano.Chain.Genesis.configGenesisData byronConfig

      networkId = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic   -> Testnet networkMagic

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo :: LocalNodeConnectInfo CardanoMode
      connectInfo =
          LocalNodeConnectInfo {
            localConsensusModeParams = cardanoModeParams,
            localNodeNetworkId       = networkId,
            localNodeSocketPath      = socketPath
          }

  c <- liftIO (client connectInfo env)

  lift $ connectToLocalNode connectInfo (protocols c)

protocols :: PipelinedLedgerStateClient -> LocalNodeClientProtocolsInMode CardanoMode
protocols client =
  LocalNodeClientProtocols {
    localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient client),
    localTxSubmissionClient = Nothing,
    localStateQueryClient   = Nothing
  }

chainSyncClient :: PipelinedLedgerStateClient -> ChainSyncClientPipelined (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
chainSyncClient PipelinedLedgerStateClient{getPipelinedLedgerStateClient} = CSP.ChainSyncClientPipelined $
  let CSP.ChainSyncClientPipelined{CSP.runChainSyncClientPipelined} = getPipelinedLedgerStateClient
  in runChainSyncClientPipelined

type ClientBlock = BlockInMode CardanoMode

fromChainTip :: ChainTip -> WithOrigin BlockNo
fromChainTip ct = case ct of
  ChainTipAtGenesis -> Origin
  ChainTip _ _ bno  -> At bno
