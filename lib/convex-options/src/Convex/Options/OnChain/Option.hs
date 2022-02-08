{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-| Plutus implementation of an option contract
-}
module Convex.Options.OnChain.Option(
  OptionRedeemer(..),
  optionContract,
  OptionType
  ) where

import Convex.Options.OnChain.Types (Option (..), OptionParam (..), OptionRedeemer (..), OptionState (..),
                                     exerciseInterval, optionValueLocked, reclaimInterval)
import Ledger (CurrencySymbol, ScriptContext (..), TokenName, TxInfo (..), TxOut (..), Value)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Contexts qualified as C
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (Datum (..))
import Plutus.V1.Ledger.Value qualified as V
import PlutusTx qualified
import PlutusTx.Prelude

{-# INLINABLE optionContract #-}
optionContract :: OptionParam -> OptionRedeemer -> ScriptContext -> Bool
optionContract OptionParam{oppOption, oppCurrency, oppState} redeemer context =
  let ScriptContext txInfo _ = context
      vs = C.valueSpent txInfo
      TxInfo{txInfoValidRange} = txInfo
  in case (oppState, redeemer, C.getContinuingOutputs context) of
    (Ready, Exercise, [TxOut{txOutValue, txOutDatumHash=Just dh}]) ->
      let newDatum = Datum (PlutusTx.toBuiltinData OptionParam{oppOption, oppCurrency, oppState = Exercised})
      in txOutValue == optionValueLocked Exercised oppOption
          && hasToken oppCurrency (opBuyerTN oppOption) vs
          && C.findDatumHash newDatum txInfo == Just dh
          && exerciseInterval oppOption `Interval.contains` txInfoValidRange
    (_, Reclaim, []) ->
      hasToken oppCurrency (opSellerTN oppOption) vs
      && reclaimInterval oppOption `Interval.contains` txInfoValidRange

{-# INLINABLE hasToken #-}
hasToken :: CurrencySymbol -> TokenName -> Value -> Bool
hasToken cs tn vl = V.valueOf vl cs tn == 1

data OptionType

instance Scripts.ValidatorTypes OptionType where
  type instance DatumType OptionType = OptionParam
  type instance RedeemerType OptionType = OptionRedeemer
