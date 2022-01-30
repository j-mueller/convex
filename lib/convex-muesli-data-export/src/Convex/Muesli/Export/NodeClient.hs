module Convex.Muesli.Export.NodeClient(exportClient) where

import Cardano.Api (Env)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Convex.Event qualified as Event
import Convex.NodeClient (CatchingUp (..), PipelinedLedgerStateClient, foldClient)
import GHC.IO.Handle (Handle)
import Convex.Muesli.Constants qualified as Constants
import Convex.Muesli.Export.CSV qualified as CSV
import Convex.Muesli.Export.Transaction (StakingCredentials)
import Convex.Muesli.Export.Transaction qualified as Transaction

exportClient ::
  StakingCredentials
  -> Handle
  -> Env
  -> PipelinedLedgerStateClient
exportClient ownStakingKeys fileHandle env = foldClient mempty env $ \catchingUp currentState currentBlock -> runMaybeT $ do
  let (newEvents, newInputs) = Event.extract Constants.muesliVersion currentState currentBlock

  traverse_ (liftIO . CSV.appendRow fileHandle) (mapMaybe (Transaction.extract ownStakingKeys) newEvents)

  guard (catchingUp == CatchingUpWithNode)
  pure newInputs

