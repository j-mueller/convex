{-# LANGUAGE NamedFieldPuns #-}
{-| Network information from local cardano node
-}
module Convex.Wallet.NodeEnv(
  PoolId,
  BalanceTxNodeEnv(..),
  GetNodeEnvError(..),
  getNodeEnv
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley (CardanoMode, LocalNodeConnectInfo, NetworkId, ProtocolParameters, QueryInEra (..),
                            QueryInShelleyBasedEra (QueryProtocolParameters), StakePoolKey, queryExpr)
import Cardano.Slotting.Time (SystemStart)
import Data.Set (Set)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

{-| Things that can go wrong when querying the node
-}
data GetNodeEnvError =
  AcquireFailureE AcquireFailure
  | EraMismatchE EraMismatch
  deriving Show

{-| Query the local node to get the 'BalanceTxNodeEnv'
-}
getNodeEnv :: LocalNodeConnectInfo CardanoMode -> IO (Either GetNodeEnvError BalanceTxNodeEnv)
getNodeEnv info =
  let eInMode = C.AlonzoEraInCardanoMode
      sbe     = C.ShelleyBasedEraAlonzo
      C.LocalNodeConnectInfo{C.localNodeNetworkId} = info
  in fmap (either (Left . AcquireFailureE) (either (Left . EraMismatchE) Right))
      $ C.executeLocalStateQueryExpr info Nothing $ \_ -> do
          bteParams <- queryExpr $ C.QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
          systemStart <- queryExpr C.QuerySystemStart
          eraHistory <- queryExpr (C.QueryEraHistory C.CardanoModeIsMultiEra)
          stakePools <- queryExpr $ C.QueryInEra eInMode $ QueryInShelleyBasedEra sbe $ C.QueryStakePools
          pure (BalanceTxNodeEnv
                  eInMode
                  systemStart
                  eraHistory
                  <$> bteParams
                  <*> stakePools
                  <*> pure localNodeNetworkId)

{-| Network information (available from cardano node)
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
type PoolId = C.Hash StakePoolKey
