{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
-}
module Convex.Options.OnChain.MintingPolicy(
  MPSRedeemer(..),
  mintingPolicy
  ) where

import Convex.Options.OnChain.Types (MPSRedeemer (..), Option (..), initialParam, optionValueLocked)
import Ledger hiding (singleton)
import Plutus.V1.Ledger.Value qualified as V
import PlutusTx qualified
import PlutusTx.Prelude

{-# INLINABLE mintingPolicy #-}
mintingPolicy :: ValidatorHash -> Option -> TxOutRef -> MPSRedeemer -> ScriptContext -> Bool
mintingPolicy optionHash option utxo redeemer (ScriptContext txInfo (Minting currencySymbol)) =
  let mnt = txInfoMint txInfo in
  case redeemer of
    MPSMinting ->
      let initP   = initialParam currencySymbol option
          Just dh = findDatumHash (Datum $ PlutusTx.toBuiltinData initP) txInfo
          [(dh', vl')] = scriptOutputsAt optionHash txInfo
      in  (spendsUtxo utxo $ txInfoInputs txInfo) &&
          (mintsToken currencySymbol (opBuyerTN option) mnt) &&
          (mintsToken currencySymbol (opSellerTN option) mnt) &&
          (mintsToken currencySymbol (opUtxoTN option) mnt) &&

          -- ensure that the option is initialised with the correct value + state
          dh' == dh && vl' == optionValueLocked initP

          -- At this point we could also validate the @option@ parameter. Check that the token names are all different,
          -- the exercise date is in the future, etc. But these checks can also be performed offline by potential
          -- buyers of the tokens. So we can save some exunits by not doing them here.

    -- burning is allowed at any time.
    MPSBurning ->
      (mayBurnToken currencySymbol (opBuyerTN option)  mnt) &&
      (mayBurnToken currencySymbol (opSellerTN option) mnt) &&
      (mayBurnToken currencySymbol (opUtxoTN option) mnt)

mintingPolicy _ _ _ _ _ = False

{-# INLINABLE spendsUtxo #-}
spendsUtxo :: TxOutRef -> [TxInInfo] -> Bool
spendsUtxo _ [] = traceError "C-s" {- SpendsUTXO failed -}
spendsUtxo txref ((TxInInfo txref' _) : tl)
  | txref == txref' = True
  | otherwise = spendsUtxo txref tl

{-# INLINABLE mintsToken #-}
mintsToken :: CurrencySymbol -> TokenName -> Value -> Bool
mintsToken cs tn vl =
  V.valueOf vl cs tn == 1

{-# INLINABLE mayBurnToken #-}
mayBurnToken :: CurrencySymbol -> TokenName -> Value -> Bool
mayBurnToken cs tn vl =
  let amt = V.valueOf vl cs tn
  in amt == 0 || amt == (-1)
