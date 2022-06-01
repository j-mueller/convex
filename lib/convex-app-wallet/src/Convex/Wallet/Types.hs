{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Convex.Wallet.Types(
  Wallet(..),
  WalletInfo(..),
  address,
  info,
  privateKey,
  paymentCredential
  ) where

import Cardano.Api (Address, NetworkId, PaymentCredential, PaymentKey, ShelleyAddr, SigningKey)
import Cardano.Api qualified as C
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Prettyprinter (Pretty (..), vsep, (<+>))

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

{-| The wallet's private key (serialised)
-}
privateKey :: Wallet -> Text
privateKey Wallet{wKey} = C.serialiseToBech32 wKey

info :: NetworkId -> Wallet -> WalletInfo
info networkId w@Wallet{wNonAdaReturnAddress} =
  WalletInfo
    { wiReturnAddress = C.serialiseToBech32 wNonAdaReturnAddress
    , wiAddress       = C.serialiseToBech32 (address networkId w)
    }

{-| Information about a wallet with serialised addresses
-}
data WalletInfo =
  WalletInfo
    { wiReturnAddress :: Text
    , wiAddress       :: Text
    }

instance ToJSON WalletInfo where
  toJSON WalletInfo{wiReturnAddress, wiAddress} =
    object
      [ "return_address" .= wiReturnAddress
      , "wallet_address" .= wiAddress
      ]

instance Pretty WalletInfo where
  pretty WalletInfo{wiReturnAddress, wiAddress} =
    vsep
      [ "Return address:" <+> pretty wiReturnAddress
      , "Wallet address:" <+> pretty wiAddress
      ]
