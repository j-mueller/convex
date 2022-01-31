{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-| Keeping track of the prices of cardano assets (relative to Ada)
-}
module Convex.Prices.AssetPrices(
  -- * Price points for a single asset
  AssetPriceData(..),
  lastTrade,
  mean,
  ewm,
  variance,
  lastTradeSlot,
  lastTradeTime,
  -- * Prices for all assets
  AssetPrices(..),
  emptyAssetPrices,
  update,
  updateTransaction
  ) where

import Cardano.Api (AssetId, SlotNo)
import Cardano.Api qualified as C
import Control.Lens (at, makeLenses, makePrisms, over)
import Convex.Muesli.Transaction (Transaction (..))
import Convex.Muesli.Transaction qualified as T
import Convex.Prices.Measures (ExponentialWeightedMean, Mean (..), Variance)
import Convex.Prices.Measures qualified as M
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)

data AssetPriceData =
  AssetPriceData
    { _lastTrade     :: !Mean
    , _mean          :: !Mean
    , _ewm           :: !ExponentialWeightedMean
    , _variance      :: !Variance
    , _lastTradeSlot :: !SlotNo
    , _lastTradeTime :: !UTCTime
    }

makeLenses ''AssetPriceData

newtype AssetPrices = AssetPrices{ unAssetPrices :: Map AssetId AssetPriceData }

makePrisms ''AssetPrices

emptyAssetPrices :: AssetPrices
emptyAssetPrices = AssetPrices mempty

update :: AssetId -> Mean -> SlotNo -> AssetPrices -> AssetPrices
update assetId mean_ slotNo =
  let md Nothing =
          Just AssetPriceData
            { _lastTrade = mean_
            , _mean = mean_
            , _ewm = M.countExponentialWeightedMean 0.95 M.emptyEwm mean_
            , _variance = M.observe M.emptyVariance mean_
            , _lastTradeSlot = slotNo
            , _lastTradeTime = T.slotToTime slotNo
            }
      md (Just oldData) =
          Just AssetPriceData
            { _lastTrade = mean_
            , _mean = _mean oldData <> mean_
            , _ewm = M.countExponentialWeightedMean 0.95 (_ewm oldData) mean_
            , _variance = M.observe (_variance oldData) mean_
            , _lastTradeSlot = slotNo
            , _lastTradeTime = T.slotToTime slotNo
            }
  in over (_AssetPrices . at assetId) md

{-| Update the prices with the data from a muesli transaction
-}
updateTransaction :: Transaction -> AssetPrices -> AssetPrices
updateTransaction Transaction{txnSlot, txnTradeOtherAssetId, txnTradeOtherAmount=C.Quantity q, txnTradeLovelaceAmount=C.Lovelace l} =
  let m = Mean{mCount = fromIntegral q, mSum = l}
  in update txnTradeOtherAssetId m txnSlot
