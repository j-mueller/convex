{-# LANGUAGE NamedFieldPuns #-}
module Convex.Wallet.Types(
  Wallet(..),
  address,
  paymentCredential
  ) where

import Cardano.Api (Address, NetworkId, PaymentCredential, PaymentKey, ShelleyAddr, SigningKey)
import Cardano.Api qualified as C

data Wallet =
  Wallet
    { wKey                 :: SigningKey PaymentKey
    , wNonAdaReturnAddress :: Address ShelleyAddr
    }

{-| The wallet's payment credential (public key)
-}
paymentCredential :: Wallet -> PaymentCredential
paymentCredential Wallet{wKey} =
  let hsh = C.verificationKeyHash (C.getVerificationKey wKey)
  in C.PaymentCredentialByKey hsh

{-| The address of the wallet
-}
address :: NetworkId -> Wallet -> Address ShelleyAddr
address networkId wallet =
  C.makeShelleyAddress networkId (paymentCredential wallet) C.NoStakeAddress
