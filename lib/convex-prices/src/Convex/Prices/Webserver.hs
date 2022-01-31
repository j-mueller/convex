{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-| Servant API and related types for querying the asset prices
-}
module Convex.Prices.Webserver(
  ServerArgs(..),
  startServer,
  APIPriceInfo(..),
  fromAssetPriceData
  ) where

import Cardano.Api (AssetId (..), SlotNo (..))
import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Prices.AssetPrices (AssetPriceData (..), AssetPrices (..))
import Convex.Prices.Measures qualified as M
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GHC.Word (Word64)
import Network.Wai.Handler.Warp qualified
import Servant (Application, Get, JSON, Server, serve, (:>))

type API = "prices" :> Get '[JSON] (Map Text APIPriceInfo)

data ServerArgs =
  ServerArgs
    { svPrices :: TVar AssetPrices
    , svPort   :: Int
    }

data APIPriceInfo =
  APIPriceInfo
    { lastPrice          :: !Double
    , meanPrice          :: !Double
    , ewMeanPrice        :: !Double
    , variance           :: !Double
    , stdDev             :: !Double
    , lastTradeSlot      :: !Word64
    , lastTradeTimestamp :: !Text
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

fromAssetPriceData :: AssetPriceData -> APIPriceInfo
fromAssetPriceData AssetPriceData{_lastTrade, _mean, _ewm, _variance, _lastTradeSlot=SlotNo lastTradeSlot, _lastTradeTime} =
  let variance = fromMaybe 0 (M.getVariance _variance)
      stdDev = sqrt variance
  in APIPriceInfo
    { lastPrice = fromMaybe 0 (M.getMean _lastTrade)
    , meanPrice = fromMaybe 0 (M.getMean _mean)
    , ewMeanPrice = fromMaybe 0 (M.getExponentialWeightedMean _ewm)
    , variance
    , stdDev
    , lastTradeSlot
    , lastTradeTimestamp = Text.pack (show _lastTradeTime)
    }

assetIdToText :: AssetId -> Text
assetIdToText AdaAssetId                 = "Ada"
assetIdToText (AssetId policy assetName) = requote (tshow policy <> "." <> tshow assetName)

tshow :: Show a => a -> Text
tshow = Text.pack . show

requote :: Text -> Text
requote txt = Text.filter ((/=) '\"') txt

server :: ServerArgs -> Server API
server ServerArgs{svPrices} =
  liftIO $ atomically $ do
    AssetPrices{unAssetPrices} <- readTVar svPrices
    pure $ Map.fromList $ fmap (bimap assetIdToText fromAssetPriceData) $ Map.toList unAssetPrices

app :: ServerArgs -> Application
app args = serve (Proxy @API) (server args)

startServer :: ServerArgs -> IO ()
startServer args@ServerArgs{svPort} =
  Network.Wai.Handler.Warp.run svPort (app args)
