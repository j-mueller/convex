{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TupleSections      #-}
module Convex.Event(
    TxWithEvents(..),
    NewOutputEvent(..),
    OutputSpentEvent(..),
    Event(..),
    ScriptOutDataHash,
    txIn,
    splitEvent,
    -- * Extraction
    ResolvedInputs(..),
    extract,
    convertScript,
    extractAlonzoTxn',
    extractAlonzoTxn
    ) where

import Cardano.Api (AlonzoEra, AssetName, Block (..), BlockHeader, BlockInMode (..), BlockNo, CardanoMode,
                    EraInMode (..), PolicyId, ScriptHash, SlotNo, Tx (..), TxId, TxIn (..), TxIx (..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley (TxBody (..))
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.Address qualified as Address
import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.Data (Data, DataHash)
import Cardano.Ledger.Alonzo.Data qualified as Alonzo.Data
import Cardano.Ledger.Alonzo.Scripts qualified as Scripts
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxWitness qualified as TxWitness
import Cardano.Ledger.Credential qualified as Credential
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era qualified as Era
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Shelley.Metadata (Metadatum)
import Cardano.Ledger.Shelley.TxBody (witKeyHash)
import Cardano.Ledger.TxIn qualified as CT
import Control.Monad.State.Strict (MonadState, get, put, runState)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldl', toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Maybe.Strict (StrictMaybe (SJust))
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Shelley.Eras (StandardAlonzo)

type ScriptOutDataHash = DataHash (Era.Crypto (Alonzo.AlonzoEra StandardCrypto))

data Currency = Ada | Native{policyId :: PolicyId, assetName :: AssetName}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Event a =
  AnOutputSpentEvent !(OutputSpentEvent a)
  | ANewOutputEvent !(NewOutputEvent a)
  deriving stock (Eq, Show, Generic)

{-| A transaction annotated with events extracted from it.
-}
data TxWithEvents a =
  TxWithEvents
    { twTx     :: !(Tx AlonzoEra)
    , twEvents :: !(NonEmpty (Event a))
    , twBlock  :: !BlockNo
    , twSlot   :: !SlotNo
    } deriving stock (Eq, Show, Generic)

splitEvent :: Event a -> Either (OutputSpentEvent a) (NewOutputEvent a)
splitEvent = \case
  AnOutputSpentEvent e -> Left e
  ANewOutputEvent e    -> Right e

data OutputSpentEvent a =
  OutputSpentEvent
      { oseTxIn       :: !TxIn
      , oseRedeemer   :: !(Data StandardAlonzo)
      , oseSpendingTx :: !TxId
      , oseTxOutput   :: !(NewOutputEvent a)
      } deriving stock (Eq, Show, Generic)

data NewOutputEvent a =
  NewOutputEvent
    { neTransaction :: !TxId
    , neEvent       :: !a
    , neTxIx        :: !TxIx
    , neOutput      :: !ScriptOut
    , neDatum       :: !(Maybe (Data (Alonzo.AlonzoEra StandardCrypto)))
    , neBlockNo     :: !Integer
    , neSlot        :: !SlotNo
    , neScriptHash  :: !ScriptHash
    , neDataHash    :: !ScriptOutDataHash
    , neSigners     :: ![KeyHash 'Witness StandardCrypto]
    , neTxMetadata  :: !(Map Word64 Metadatum)
    } deriving stock (Eq, Show, Generic)

{-| The 'TxIn' of the new output
-}
txIn :: NewOutputEvent a -> TxIn
txIn NewOutputEvent{neTransaction, neTxIx} = TxIn neTransaction neTxIx

newtype ResolvedInputs a = ResolvedInputs (Map TxIn (NewOutputEvent a))
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

extract ::
  (ScriptHash -> Maybe a)
  -> ResolvedInputs a -- ^ Resolved inputs
  -> BlockInMode CardanoMode -- ^ New block
  -> ([TxWithEvents a], ResolvedInputs a) -- ^ Defi events extracted from block
extract ex resolvedInputs = \case
  BlockInMode block AlonzoEraInCardanoMode -> extractAlonzoBlock ex resolvedInputs block
  _                                        -> ([], resolvedInputs)

extractAlonzoBlock :: (ScriptHash -> Maybe a) -> ResolvedInputs a -> Block AlonzoEra -> ([TxWithEvents a], ResolvedInputs a)
extractAlonzoBlock ex resolvedInputs (Block blockHeader txns) =
  first catMaybes $ flip runState resolvedInputs $ traverse (extractAlonzoTxn ex blockHeader) txns

type ScriptOut = Alonzo.TxBody.TxOut (Alonzo.AlonzoEra StandardCrypto)

extractAlonzoTxn' :: ResolvedInputs a -> (ScriptHash -> Maybe a) -> BlockHeader -> Tx AlonzoEra -> ([TxWithEvents a], ResolvedInputs a)
extractAlonzoTxn' resolvedInputs ex blockHeader tx =
  first maybeToList $ runState (extractAlonzoTxn ex blockHeader tx) resolvedInputs

extractAlonzoTxn :: forall a m. MonadState (ResolvedInputs a) m => (ScriptHash -> Maybe a) -> BlockHeader -> Tx AlonzoEra -> m (Maybe (TxWithEvents a))
extractAlonzoTxn ex (C.BlockHeader slotNo _ twBlock@(C.BlockNo blockNo)) twTx@(Tx txBody keyWitnesses) = do
  ResolvedInputs resolvedInputs <- get
  let txId = C.getTxId txBody
      ShelleyTxBody _ txBody' _scripts scriptData auxiliaryData _ = txBody
      Alonzo.TxBody.TxBody{Alonzo.TxBody.outputs, Alonzo.TxBody.inputs} = txBody'
      TxWitness.TxDats' txDats = case scriptData of
        C.TxBodyScriptData C.ScriptDataInAlonzoEra txDats' _ -> txDats'
        _                                                    -> mempty
      txReds = case scriptData of
        C.TxBodyScriptData C.ScriptDataInAlonzoEra _ (TxWitness.Redeemers txReds') -> txReds'
        _                                                                          -> mempty

      mapOutput :: TxIx -> ScriptOut -> Maybe (ScriptHash, ScriptOut, TxIx, ScriptOutDataHash, a)
      mapOutput ix (scriptOut@(Alonzo.TxBody.TxOut address _value (SJust dataHash))) = case address of
          Address.Addr _network paymentCredential _stakeReference -> case paymentCredential of
              Credential.ScriptHashObj hsh ->
                  let hsh' = CS.fromShelleyScriptHash hsh in
                  case ex hsh' of
                    Just a  -> Just (hsh', scriptOut, ix, dataHash, a)
                    Nothing -> Nothing
              _ -> Nothing
          _ -> Nothing
      mapOutput _ _ = Nothing

      relevantOutputs = mapMaybe (uncurry mapOutput) (zip (toEnum <$> [0..]) (toList outputs)) -- $ zip (toList outputs) (toList txOuts))

      outputEvents :: [NewOutputEvent a]
      outputEvents = fmap mkEvent relevantOutputs where
        mkEvent (neScriptHash, neOutput, neTxIx, neDataHash, neEvent) =
            let neDatum = Map.lookup neDataHash txDats
            in NewOutputEvent
                { neTransaction = txId
                , neTxIx
                , neEvent
                , neOutput
                , neBlockNo = fromIntegral blockNo
                , neSlot = slotNo
                , neScriptHash
                , neDatum
                , neDataHash
                , neSigners = mapMaybe getKeyWitness keyWitnesses
                , neTxMetadata = maybe mempty (\(Alonzo.Data.AuxiliaryData meta _) -> meta) auxiliaryData
                }

      outputSpentEvents :: [OutputSpentEvent a]
      outputSpentEvents =
        -- there should always be a redeemer
        -- because we only look at inputs that spend script outputs
        fmap (\(idx, oseTxIn, oseTxOutput) -> maybe (error $ "outputSpentEvents: Redeemer not found: " <> show idx) (\(oseRedeemer, _) -> OutputSpentEvent{oseTxIn, oseTxOutput, oseRedeemer, oseSpendingTx=txId}) $ Map.lookup (TxWitness.RdmrPtr Scripts.Spend idx) txReds)
        $ mapMaybe (\(idx, oseTxIn) -> fmap (idx, oseTxIn,) $ Map.lookup oseTxIn resolvedInputs)
        $ zip [0..]
        $ fmap (uncurry TxIn . first CS.fromShelleyTxId . second (TxIx . fromIntegral) . (\(CT.TxIn i n) -> (i, n)))
        $ sortOn id
        $ Set.toList inputs

      resolvedInputsDeleted = ResolvedInputs (foldl' (\inp OutputSpentEvent{oseTxIn} -> Map.delete oseTxIn inp) resolvedInputs outputSpentEvents)

      newResolvedInputs = ResolvedInputs $ Map.fromList $ fmap (\e@NewOutputEvent{neTransaction, neTxIx} -> (TxIn neTransaction neTxIx, e)) outputEvents
  put (newResolvedInputs <> resolvedInputsDeleted)
  let newEvents = fmap ANewOutputEvent outputEvents ++ fmap AnOutputSpentEvent outputSpentEvents
  case newEvents of
    []     -> pure Nothing
    (y:ys) -> return (Just TxWithEvents{twTx, twEvents = y :| ys, twSlot = slotNo, twBlock })

convertScript :: Scripts.Script (Alonzo.AlonzoEra StandardCrypto) -> Maybe (C.Script C.PlutusScriptV1)
convertScript = \case
    Scripts.TimelockScript{}          -> Nothing
    Scripts.PlutusScript _language bs -> Just (C.PlutusScript C.PlutusScriptV1 (CS.PlutusScriptSerialised bs))

getKeyWitness :: C.KeyWitness AlonzoEra -> Maybe (KeyHash 'Witness StandardCrypto)
getKeyWitness = \case
  CS.ShelleyKeyWitness _era withVKey -> Just $ witKeyHash withVKey
  _                                  -> Nothing
