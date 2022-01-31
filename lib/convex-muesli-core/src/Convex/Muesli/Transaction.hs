{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
module Convex.Muesli.Transaction(
  StakingCredentials,
  MatchMaker(..),
  Transaction(..),
  extract,
  slotToTime
  ) where

import Cardano.Api (AssetId (..), BlockNo, Lovelace, Quantity (..), SlotNo (..), TxId, TxIn (..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Crypto qualified as Crypto
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import Convex.Event (TxWithEvents (..))
import Convex.Event qualified as E
import GHC.Generics (Generic)
import Convex.Muesli.Constants (MuesliVersion)
import Convex.Muesli.Match qualified as M

data MatchMaker = Self | Other
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{-| A muesli swap transaction
-}
data Transaction =
  Transaction
    { txnSlot                 :: SlotNo
    , txnBlock                :: BlockNo
    , txnTimestamp            :: UTCTime
    , txnId                   :: TxId
    , txnFee                  :: Lovelace
    , txnMatchmaker           :: MatchMaker
    , txnMatchmakerRewards    :: Lovelace
    , txnTradeLovelaceAmount  :: Lovelace
    , txnTradeOtherAssetId    :: AssetId
    , txnTradeOtherAmount     :: Quantity
    , txnMatchInputOne        :: TxIn
    , txnMatchInputTwo        :: TxIn
    , txnMatchInputOneCreated :: SlotNo
    , txnMatchInputTwoCreated :: SlotNo
    , txnFulfilmentTime       :: SlotNo
    , txnTimeToMatch          :: SlotNo
    } deriving stock (Eq, Show, Generic)

extract :: StakingCredentials -> TxWithEvents MuesliVersion -> Maybe Transaction
extract ownStakingKeys TxWithEvents{twTx, twEvents, twBlock, twSlot} =
  case fst $ partitionEithers (fmap E.splitEvent $ toList twEvents) of
    [output1Spent, output2Spent] -> do
      match <- M.matchFromOutputSpentEvents output1Spent output2Spent
      let C.Tx body _ = twTx
          C.TxBody C.TxBodyContent{C.txFee=C.TxFeeExplicit _ txnFee, C.txOuts} = body
          valuePaid = M.valuePaid match
          txnMatchInputOneCreated = E.neSlot (E.oseTxOutput output1Spent)
          txnMatchInputTwoCreated = E.neSlot (E.oseTxOutput output2Spent)
      (txnTradeOtherAssetId, txnTradeOtherAmount) <- case C.valueToList (C.filterValue (/= C.AdaAssetId) valuePaid) of
        [x] -> pure x
        _   -> Nothing
      return Transaction
        { txnSlot = twSlot
        , txnBlock = twBlock
        , txnId = C.getTxId body
        , txnFee
        , txnTimestamp = slotToTime twSlot
        , txnMatchmaker = if any (isOwnTxOut ownStakingKeys) txOuts then Self else Other
        , txnMatchmakerRewards = C.selectLovelace (M.matchValue match) - txnFee
        , txnTradeLovelaceAmount = C.selectLovelace valuePaid
        , txnTradeOtherAmount
        , txnTradeOtherAssetId
        , txnMatchInputOne = E.oseTxIn output1Spent
        , txnMatchInputTwo = E.oseTxIn output2Spent
        , txnMatchInputOneCreated
        , txnMatchInputTwoCreated
        , txnFulfilmentTime = twSlot - max txnMatchInputOneCreated txnMatchInputTwoCreated
        , txnTimeToMatch = max txnMatchInputTwoCreated txnMatchInputOneCreated - min txnMatchInputOneCreated txnMatchInputTwoCreated
        }
    _                                 -> Nothing

slotToTime :: SlotNo -> UTCTime
slotToTime (SlotNo s) =
  -- TODO: Convert slot to UTC time using Cardano.Wallet.Primitive.Slotting
  let seconds = fromIntegral s - 39916975
      -- 12 September 2021 21:47:46
      alonzoStart =
        UTCTime
          { utctDay=fromGregorian 2021 9 12
          , utctDayTime = 46 + 47 * 60 + 21 * 60 * 60
          }
  in addUTCTime (secondsToNominalDiffTime seconds) alonzoStart

type StakingCredentials = Set (L.StakeReference Crypto.StandardCrypto)

isOwnTxOut :: StakingCredentials -> C.TxOut C.CtxTx C.AlonzoEra -> Bool
isOwnTxOut ownKeys (C.TxOut (C.AddressInEra _ (C.ShelleyAddress _ _ cred)) _ _) = cred `Set.member` ownKeys
isOwnTxOut _ _                                                                  = False
