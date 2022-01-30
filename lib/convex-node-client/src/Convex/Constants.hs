{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Some useful constants related to the cardano network.
-}
module Convex.Constants(
  alonzoMainnet,
  ) where

import Cardano.Api (AsType (AsHash), BlockHeader, ChainPoint (..))
import Cardano.Api qualified as CAPI
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))

-- | Start of the Alonzo era on mainnet.
-- https://explorer.cardano.org/en/block?id=8959c0323b94cc670afe44222ab8b4e72cfcad3b5ab665f334bbe642dc6e9ef4
alonzoMainnet :: ChainPoint
alonzoMainnet = fromMaybe (error "alonzoMainnet") $ do
  hsh <- CAPI.deserialiseFromRawBytesHex (AsHash (CAPI.proxyToAsType (Proxy :: Proxy BlockHeader))) "8959c0323b94cc670afe44222ab8b4e72cfcad3b5ab665f334bbe642dc6e9ef4"
  return $ ChainPoint 39_916_975 hsh
