{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-| Statistics about transactions that we process in the wallet
-}
module Convex.Wallet.Stats(
  Stats(..),
  transactionsBalancedAndSent,
  transactionsBalancingFailed,
  transactionsInputSpent,
  transactionsOnLedger,
  transactionsTimeout,

  -- * Counting and measuring
  txnReceived,

  -- * Stats collected over time
  WalletStats(..),
  StatsAt(..),
  prepend,
  emptyStats,
  fromBlockHeader,
  splitLastNSlots,
  lastHour,
  lastDay,
  lastWeek,
  stats,
  toList
  ) where

import Cardano.Api (BlockHeader, BlockNo, Hash, SlotNo)
import Cardano.Api qualified as C
import Control.Lens (makeLenses, (&), (.~))
import Data.Aeson (FromJSON, ToJSON)
import Data.FingerTree (FingerTree, Measured (..), split, (<|))
import Data.Foldable qualified as F
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import GHC.Generics (Generic)

data Stats =
  Stats
    { _transactionsReceived        :: !Int -- ^ How many transactions were posted by the user (total)
    , _transactionsBalancingFailed :: !Int -- ^ How many transactions failed to balance
    , _transactionsBalancedAndSent :: !Int -- ^ How many transactions were balanced and sent to the network
    , _transactionsOnLedger        :: !Int -- ^ How many transactions have appeared on the ledger (successfully)
    , _transactionsTimeout         :: !Int -- ^ How many transactions were rejected after their validity interval was exceeded
    , _transactionsInputSpent      :: !Int -- ^ How many txns were rejected after one of their inputs was spent by a different transaction

    , _slotRange                   :: !(StrictMaybe (SlotNo, SlotNo))

    } deriving stock (Eq, Ord, Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

instance Semigroup Stats where
  l <> r =
    Stats
      { _transactionsReceived = _transactionsReceived l + _transactionsReceived r
      , _transactionsBalancingFailed = _transactionsBalancingFailed l + _transactionsBalancingFailed r
      , _transactionsBalancedAndSent = _transactionsBalancedAndSent l + _transactionsBalancedAndSent r
      , _transactionsOnLedger = _transactionsOnLedger l + _transactionsOnLedger r
      , _transactionsTimeout = _transactionsTimeout l + _transactionsTimeout r
      , _transactionsInputSpent = _transactionsInputSpent l + _transactionsInputSpent r
      , _slotRange =
          case (_slotRange l, _slotRange r) of
            (SNothing, x)                            -> x
            (x, SNothing)                            -> x
            (SJust (fromL, toL), SJust (fromR, toR)) -> SJust (min fromL fromR, max toL toR)
      }

instance Monoid Stats where
  mempty = Stats 0 0 0 0 0 0 SNothing

makeLenses ''Stats

-- When counting events we should make sure to record entry and exit point for every transaction.
-- That is, we count wsTransactionsReceived and exactly one of
-- wsTransactionsBalancingFailed, wsTransactionsBalancedAndSent for every transaction.

txnReceived :: Stats
txnReceived = mempty & transactionsReceived .~ 1

data StatsAt =
  StatsAt
    { _blockNo   :: BlockNo
    , _slotNo    :: SlotNo
    , _blockHash :: Hash BlockHeader
    , _stats     :: Stats
    } deriving stock (Show)

fromBlockHeader :: BlockHeader -> Stats -> StatsAt
fromBlockHeader (C.BlockHeader _slotNo _blockHash _blockNo) s =
  let _stats = s & slotRange .~ SJust (_slotNo, _slotNo)
  in StatsAt{_blockNo, _slotNo, _blockHash, _stats}

instance Measured Stats StatsAt where
  measure = _stats

newtype WalletStats = WalletStats{ unWalletStats :: FingerTree Stats StatsAt }
  deriving stock (Show)

prepend ::  StatsAt -> WalletStats -> WalletStats
prepend s WalletStats{unWalletStats} =
  WalletStats $ s <| unWalletStats

emptyStats :: WalletStats
emptyStats = WalletStats mempty

splitLastNSlots :: Int -> WalletStats -> (WalletStats, WalletStats)
splitLastNSlots n (WalletStats s) =
  case (_slotRange $ measure s) of
    SNothing -> (WalletStats mempty, WalletStats s)
    SJust (_, maxSlot) ->
      let cutoffPoint :: SlotNo = maxSlot - (fromIntegral n)
          f Stats{_slotRange = SNothing}            = True
          f Stats{_slotRange = SJust (minSlot', _)} = minSlot' < cutoffPoint
          (this, that) = split f s
      in (WalletStats this, WalletStats that)

-- | Split a 'WalletStats' value into one with the values for the last hour
--   and one with the rest
lastHour :: WalletStats -> (WalletStats, WalletStats)
lastHour = splitLastNSlots (60 * 60)

-- | Split a 'WalletStats' value into one with the values for the last day
--   and one with the rest
lastDay :: WalletStats -> (WalletStats, WalletStats)
lastDay = splitLastNSlots (60 * 60 * 24)

-- | Split a 'WalletStats' value into one with the values for the last week
--   and one with the rest
lastWeek :: WalletStats -> (WalletStats, WalletStats)
lastWeek = splitLastNSlots (60 * 60 * 24 * 7)

-- | The raw stats
stats :: WalletStats -> Stats
stats = measure . unWalletStats

-- | A list with all stats that were recorded
toList :: WalletStats -> [StatsAt]
toList = F.toList . unWalletStats
