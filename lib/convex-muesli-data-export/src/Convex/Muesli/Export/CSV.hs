{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Convex.Muesli.Export.CSV(
  ToRow(..),
  appendRow,
  writeHeaders
  ) where

import Cardano.Api (AssetId)
import Cardano.Api qualified as C
import Convex.Muesli.Transaction (Transaction (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Handle (Handle)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 qualified as F

class ToRow a where
  header :: Proxy a -> [Text]
  row :: a -> [Text]

appendRow :: ToRow a => Handle -> a -> IO ()
appendRow handle vl =
  let rw = T.intercalate ";" (row vl)
  in T.hPutStrLn handle rw

writeHeaders :: ToRow a => Proxy a -> Handle -> IO ()
writeHeaders p handle =
  let rw = T.intercalate ";" (header p)
  in T.hPutStrLn handle rw

instance ToRow Transaction where
  header _ =
    [ "slot"
    , "block"
    , "timestamp"
    , "tx"
    , "txfee"
    , "matchmaker"
    , "matchmaker_rewards"
    , "trade_lovelace"
    , "trade_asset_id"
    , "trade_asset_amount"
    , "trade_input_one"
    , "trade_input_two"
    , "trade_input_one_created"
    , "trade_input_two_created"
    , "fulfilment_time"
    , "time_to_match"
    ]
  row Transaction{txnSlot, txnBlock=C.BlockNo b, txnTimestamp, txnId, txnFee=C.Lovelace fee, txnMatchmaker, txnMatchmakerRewards=C.Lovelace rewards, txnTradeLovelaceAmount=C.Lovelace amt, txnTradeOtherAssetId,txnTradeOtherAmount=C.Quantity otherAmount, txnMatchInputOne, txnMatchInputTwo, txnMatchInputOneCreated, txnMatchInputTwoCreated, txnFulfilmentTime, txnTimeToMatch} =
    [ tSlot txnSlot
    , tshow b
    , tTimestamp txnTimestamp
    , tshow txnId
    , tshow fee
    , tshow txnMatchmaker
    , tshow rewards
    , tshow amt
    , tAssetId txnTradeOtherAssetId
    , tshow otherAmount
    , tTxIn txnMatchInputOne
    , tTxIn txnMatchInputTwo
    , tSlot txnMatchInputOneCreated
    , tSlot txnMatchInputTwoCreated
    , tSlot txnFulfilmentTime
    , tSlot txnTimeToMatch
    ]

tshow :: Show a => a -> Text
tshow = requote . Text.pack . show

requote :: Text -> Text
requote txt = Text.filter ((/=) '\"') txt

tAssetId :: AssetId -> Text
tAssetId = \case
  C.AdaAssetId               -> "Ada"
  C.AssetId policy assetName -> requote (tshow policy <> "." <> tshow assetName)

tTimestamp :: UTCTime -> Text
tTimestamp = Text.pack . F.iso8601Show

tTxIn :: C.TxIn -> Text
tTxIn (C.TxIn txId (C.TxIx ix)) = requote (tshow txId <> ":" <> tshow ix)

tSlot :: C.SlotNo -> Text
tSlot (C.SlotNo s) = Text.pack (show s)
