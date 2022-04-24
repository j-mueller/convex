{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

{-| Utilities for convex-app-wallet
-}
module Convex.Wallet.Utils(
  spentTxIns,
  txOutValue,
  txOutValue',
  hasNonAdaAssets,
  addKeyInput,
  addKeyInputs,
  addPublicKeyOutput,
  addPublicKeyOutput',
  addCollateral,
  setCollateral,
  setProtocolParams,
  TxOut
  ) where

import Cardano.Api (Address, AlonzoEra, Block (..), BlockInMode (..), CardanoMode, EraInMode (AlonzoEraInCardanoMode),
                    Lovelace, ShelleyAddr, Tx (..), TxId, TxIn (..), TxIx (..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley (BuildTx, ProtocolParameters, TxBody (..), TxBodyContent, Value)
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo.TxBody
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.TxIn qualified as CT
import Data.Bifunctor (Bifunctor (..))
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

spentTxIns :: BlockInMode CardanoMode -> [(TxIn, TxId)]
spentTxIns = \case
  BlockInMode block AlonzoEraInCardanoMode -> spentTxInsAlonzoBlock block
  _                                        -> []

spentTxInsAlonzoBlock :: Block AlonzoEra -> [(TxIn, TxId)]
spentTxInsAlonzoBlock (Block _blockHeader txns) =
  foldMap txIns txns

txIns :: Tx AlonzoEra -> [(TxIn, TxId)]
txIns (Tx txBody _) =
  let txId = C.getTxId txBody
      ShelleyTxBody _ txBody' _scripts _scriptData _auxiliaryData _ = txBody
      Alonzo.TxBody.TxBody{Alonzo.TxBody.inputs} = txBody'
  in fmap ((,txId) . uncurry TxIn . bimap CS.fromShelleyTxId (TxIx . fromIntegral) . (\(CT.TxIn i n) -> (i, n)))
        $ Set.toList inputs

type TxOut = Alonzo.TxBody.TxOut (Alonzo.AlonzoEra StandardCrypto)

{-| The 'Value' of a tx out
-}
txOutValue :: Alonzo.TxBody.TxOut (Alonzo.AlonzoEra StandardCrypto) -> Value
txOutValue x = case CS.fromShelleyTxOut CS.ShelleyBasedEraAlonzo x of
  CS.TxOut _ (CS.TxOutValue _ vl) _ -> vl

txOutValue' :: CS.TxOutValue AlonzoEra -> Value
txOutValue' (CS.TxOutValue _ x) = x

{-| Returns True if the 'Value' has any non-Ada (native) assets
-}
hasNonAdaAssets :: Value -> Bool
hasNonAdaAssets v = case filter ((/= 0) . snd) (CS.valueToList v) of
  []                   -> False
  [(CS.AdaAssetId, _)] -> False
  _                    -> True

{-| Add an input spending a public-key output to the tx body
-}
addKeyInput :: TxIn -> TxOut -> TxBodyContent BuildTx AlonzoEra -> TxBodyContent BuildTx AlonzoEra
addKeyInput txIn txOut content  =
  let oldTxIns = C.txIns content
      newTxIns = (txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) : oldTxIns
  in content{C.txIns = newTxIns}

{-| Add some public key inputs to the tx body
-}
addKeyInputs :: TxBodyContent BuildTx AlonzoEra -> [(TxIn, TxOut)] -> TxBodyContent BuildTx AlonzoEra
addKeyInputs = foldl' (\k (txIn, txOut) -> addKeyInput txIn txOut k)

{-| Add a public key output with the given lovelace value to the tx body
-}
addPublicKeyOutput :: Address ShelleyAddr -> Lovelace -> TxBodyContent BuildTx AlonzoEra ->  TxBodyContent BuildTx AlonzoEra
addPublicKeyOutput address lovelace = addPublicKeyOutput' address (CS.lovelaceToValue lovelace)

{-| Add a public key output with the given lovelace value to the tx body
-}
addPublicKeyOutput' :: Address ShelleyAddr -> Value -> TxBodyContent BuildTx AlonzoEra ->  TxBodyContent BuildTx AlonzoEra
addPublicKeyOutput' address vl body =
  let addr' = CS.AddressInEra (CS.ShelleyAddressInEra CS.ShelleyBasedEraAlonzo) address
      txOut = CS.TxOut addr' (CS.TxOutValue CS.MultiAssetInAlonzoEra vl) CS.TxOutDatumNone
      oldTxOuts = C.txOuts body
  in body{C.txOuts = oldTxOuts ++ [txOut]}

{-| Add the given 'TxOut' as collateral to the transaction
-}
addCollateral :: TxIn -> TxBodyContent BuildTx AlonzoEra -> TxBodyContent BuildTx AlonzoEra
addCollateral x body =
  let oldCollateral = CS.txInsCollateral body
      CS.TxInsCollateral w xs = oldCollateral
      newCollateral = CS.TxInsCollateral w (x:xs)
  in body{CS.txInsCollateral = newCollateral}

{-| Add the given 'TxOut' as collateral to the transaction
-}
setCollateral :: [TxIn] -> TxBodyContent BuildTx AlonzoEra -> TxBodyContent BuildTx AlonzoEra
setCollateral xs body =
  let oldCollateral = CS.txInsCollateral body
      CS.TxInsCollateral w _ = oldCollateral
      newCollateral = CS.TxInsCollateral w xs
  in body{CS.txInsCollateral = newCollateral}

setProtocolParams :: ProtocolParameters -> TxBodyContent BuildTx AlonzoEra -> TxBodyContent BuildTx AlonzoEra
setProtocolParams pp bd = bd{C.txProtocolParams = C.BuildTxWith $ Just pp}
