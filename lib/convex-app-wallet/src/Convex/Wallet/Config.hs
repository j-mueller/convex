{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeFamilies       #-}
module Convex.Wallet.Config(
  ConfigMode(..),
  ConfigError(..),
  Config(..),
  configParser,
  mkTyped
  ) where

import Cardano.Api (Address, PaymentKey, ShelleyAddr, SigningKey)
import Cardano.Api qualified as C
import Data.Bifunctor (Bifunctor (..))
import Data.Text qualified as Text
import Options.Applicative (Parser, help, long, strOption)

data ConfigMode = Str | Typed

type family ReturnAddress t where
  ReturnAddress 'Str = String
  ReturnAddress 'Typed = Address ShelleyAddr

type family WalletKey t where
  WalletKey 'Str = String
  WalletKey 'Typed = SigningKey PaymentKey

data Config (m :: ConfigMode) =
  Config
    { cardanoNodeConfigFile :: FilePath
    , cardanoNodeSocket     :: FilePath
    , nonAdaReturnAddress   :: ReturnAddress m
    , walletKey             :: WalletKey m
    }

deriving stock instance Eq (Config 'Str)
deriving stock instance Ord (Config 'Str)
deriving stock instance Show (Config 'Str)
deriving stock instance Show (Config 'Typed)

configParser :: Parser (Config 'Str)
configParser =
  Config
    <$> strOption (long "node-config" <> help "Cardano node config JSON file")
    <*> strOption (long "node-socket" <> help "Cardano node socket")
    <*> strOption (long "return-address" <> help "Return address for non-Ada values. Does not need to belong to this wallet")
    <*> strOption (long "wallet-key" <> help "Serialised private key of the wallet")

data ConfigError =
  ParseAddressError C.Bech32DecodeError
  | ParseKeyError C.Bech32DecodeError
  deriving Show

mkTyped :: Config 'Str -> Either ConfigError (Config 'Typed)
mkTyped Config{cardanoNodeConfigFile, cardanoNodeSocket, nonAdaReturnAddress, walletKey} = do
  key <- first ParseKeyError (C.deserialiseFromBech32 (C.AsSigningKey C.AsPaymentKey) (Text.pack walletKey))
  addr <- first ParseAddressError (C.deserialiseFromBech32 (C.AsAddress C.AsShelleyAddr) (Text.pack nonAdaReturnAddress))
  pure Config{cardanoNodeSocket, cardanoNodeConfigFile, walletKey = key, nonAdaReturnAddress = addr}
