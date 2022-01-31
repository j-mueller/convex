module Convex.Prices.NodeClient(pricesClient) where

import Cardano.Api (Env)
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Event qualified as Event
import Convex.Muesli.Constants qualified as Constants
import Convex.Muesli.Transaction qualified as Transaction
import Convex.NodeClient (PipelinedLedgerStateClient, foldClient)
import Convex.Prices.AssetPrices (AssetPrices)
import Convex.Prices.AssetPrices qualified as Prices
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)

pricesClient ::
  TVar AssetPrices
  -> Env
  -> PipelinedLedgerStateClient
pricesClient assetPrices env = foldClient mempty env $ \_ currentState currentBlock -> do
  let (newEvents, newInputs) = Event.extract Constants.muesliVersion currentState currentBlock
      newTransactions = mapMaybe (Transaction.extract mempty) newEvents
      update ap = foldl' (flip Prices.updateTransaction) ap newTransactions

  liftIO $ atomically $ modifyTVar' assetPrices update

  pure (Just newInputs)
