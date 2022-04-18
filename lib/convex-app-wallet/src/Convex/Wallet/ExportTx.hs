{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.Wallet.ExportTx(
  ExportTx(..),
  ExportTxInput(..),
  ExportTxRedeemer(..),
  -- * Constructing inputs and outputs
  redeemWith,
  input
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Data (getPlutusData)
import Data.Aeson (ToJSON (..), Value (String), object, (.=))
import Data.Aeson.Extras qualified as JSON
import Data.Maybe (mapMaybe)
import Convex.Event (NewOutputEvent (..))
import Ouroboros.Consensus.Shelley.Eras (StandardAlonzo)

-- | Partial transaction that can be balanced by the wallet.
data ExportTx =
        ExportTx
            { partialTx :: C.Tx C.AlonzoEra -- ^ The transaction itself
            , lookups   :: [ExportTxInput] -- ^ The tx outputs for all inputs spent by the partial tx
            , redeemers :: [ExportTxRedeemer]
            }

data ExportTxInput =
    ExportTxInput
        { etxiId               :: C.TxId
        , etxiTxIx             :: C.TxIx
        , etxiAddress          :: C.AddressInEra C.AlonzoEra
        , etxiLovelaceQuantity :: C.Lovelace
        , etxiDatumHash        :: Maybe (C.Hash C.ScriptData)
        , etxiAssets           :: [(C.PolicyId, C.AssetName, C.Quantity)]
        }
    deriving stock (Eq, Show)

instance ToJSON ExportTxInput where
    toJSON ExportTxInput{etxiId, etxiTxIx, etxiLovelaceQuantity, etxiDatumHash, etxiAssets, etxiAddress} =
        object
            [ "id" .= etxiId
            , "index" .= etxiTxIx
            , "address" .= C.serialiseAddress etxiAddress
            , "amount" .= object ["quantity" .= etxiLovelaceQuantity, "unit" .= ("lovelace" :: String)]
            , "datum" .= etxiDatumHash
            , "assets" .= fmap (\(p, a, q) -> object ["policy_id" .= p, "asset_name" .= a, "quantity" .= q]) etxiAssets
            ]

-- IMPORTANT: The JSON produced here needs to match the schema expected by
-- https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/balanceTransaction
instance ToJSON ExportTx where
    toJSON ExportTx{partialTx, lookups, redeemers} =
        object
            [ "transaction" .= JSON.encodeByteString (C.serialiseToCBOR partialTx)
            , "inputs"      .= lookups
            , "redeemers"   .= redeemers
            ]

data ExportTxRedeemerPurpose = Spending | Minting | Rewarding

instance ToJSON ExportTxRedeemerPurpose where
    toJSON = \case
        Spending  -> String "spending"
        Minting   -> String "minting"
        Rewarding -> String "rewarding"

data ExportTxRedeemer =
    SpendingRedeemer{ redeemer:: C.ScriptRedeemer, redeemerTxIn :: C.TxIn }
    deriving stock (Eq, Show)

instance ToJSON ExportTxRedeemer where
    toJSON SpendingRedeemer{redeemer, redeemerTxIn=C.TxIn txId txIx} =
      let d' = C.toAlonzoData @StandardAlonzo redeemer
          d = getPlutusData d'
      in object ["purpose" .= Spending, "data" .= JSON.JSONViaSerialise d, "input" .= object ["id" .= txId, "index" .= txIx]]

redeemWith :: NewOutputEvent a -> C.ScriptRedeemer -> ExportTxRedeemer
redeemWith NewOutputEvent{neTransaction, neTxIx} redeemer =
    let redeemerTxIn = C.TxIn neTransaction neTxIx
    in SpendingRedeemer{redeemer, redeemerTxIn}

input :: NewOutputEvent a -> ExportTxInput
input NewOutputEvent{neTransaction, neTxIx, neOutput} =
    let C.TxOut address (C.TxOutValue _ value) datumHash = C.fromShelleyTxOut C.ShelleyBasedEraAlonzo neOutput
        dh = case datumHash of
            C.TxOutDatumNone                             -> Nothing
            C.TxOutDatum _ scriptData                    -> Just (C.hashScriptData scriptData)
            C.TxOutDatumHash C.ScriptDataInAlonzoEra hsh -> Just hsh
        selectNonAda (C.AdaAssetId, _)                      = Nothing
        selectNonAda (C.AssetId policy assetName, quantity) = Just (policy, assetName, quantity)
        assets = mapMaybe selectNonAda (C.valueToList value)
    in ExportTxInput
        { etxiId               = neTransaction -- :: C.TxId
        , etxiTxIx             = neTxIx -- :: C.TxIx
        , etxiAddress          = address -- :: C.AddressInEra C.AlonzoEra
        , etxiLovelaceQuantity = C.selectLovelace value -- :: C.Lovelace
        , etxiDatumHash        = dh -- :: Maybe (C.Hash C.ScriptData)
        , etxiAssets           = assets -- :: [(C.PolicyId, C.AssetName, C.Quantity)]
        }
