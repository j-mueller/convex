{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-| Validator and minting scripts for options
-}
module Convex.Options.OnChain.Scripts(
  -- * Minting scripts
  mintingPolicy,
  mpsHash,
  mintingPolicySymbol,
  mintingPlutusScript,
  mintingValidator,
  serializedNftScript,

  -- * Validator script
  typedValidator,
  optionValidator,
  optionAddress,
  optionValidatorHash,
  optionScript
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Convex.Options.OnChain.MintingPolicy qualified as MPS
import Convex.Options.OnChain.Option (OptionType)
import Convex.Options.OnChain.Option qualified as Option
import Convex.Options.OnChain.Types (Option, OptionParam, OptionRedeemer)
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS
import Ledger hiding (singleton)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)

typedValidator :: Scripts.TypedValidator OptionType
typedValidator = Scripts.mkTypedValidator @OptionType
    $$(PlutusTx.compile [|| Option.optionContract ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @OptionParam @OptionRedeemer

optionValidator :: Validator
optionValidator = Scripts.validatorScript typedValidator

optionAddress :: Address
optionAddress = Scripts.validatorAddress typedValidator

optionValidatorHash :: ValidatorHash
optionValidatorHash = Scripts.validatorHash typedValidator

optionScript :: Script
optionScript = Ledger.unValidatorScript optionValidator

mintingPolicy :: Option -> TxOutRef -> MintingPolicy
mintingPolicy option txOutRef = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \val' opt' txOutRef' -> Scripts.wrapMintingPolicy $ MPS.mintingPolicy val' opt' txOutRef' ||])
  `PlutusTx.applyCode` PlutusTx.liftCode optionValidatorHash
  `PlutusTx.applyCode` PlutusTx.liftCode option
  `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

mpsHash :: Option -> TxOutRef -> MintingPolicyHash
mpsHash option utxo = mintingPolicyHash $ mintingPolicy option utxo

mintingPolicySymbol :: Option -> TxOutRef -> CurrencySymbol
mintingPolicySymbol option utxo = mpsSymbol $ mpsHash option utxo

mintingPlutusScript :: Option -> TxOutRef -> Script
mintingPlutusScript option utxo = unMintingPolicyScript $ mintingPolicy option utxo

mintingValidator :: Option -> TxOutRef -> Validator
mintingValidator option utxo = Validator $ mintingPlutusScript option utxo

serializedNftScript :: Option -> TxOutRef -> PlutusScript PlutusScriptV1
serializedNftScript option utxo = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ serialise $ mintingValidator option utxo

