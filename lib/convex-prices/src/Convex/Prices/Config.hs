{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Convex.Prices.Config(
  Config(..),
  configParser
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Options.Applicative (Parser, auto, help, long, option, strOption, value)

data Config =
  Config
      { cardanoNodeConfigFile :: FilePath
      , cardanoNodeSocket     :: FilePath
      , webserverPort         :: Int
      }
      deriving stock (Eq, Ord, Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

configParser :: Parser Config
configParser =
  Config
      <$> strOption (long "node-config" <> help "Cardano node config JSON file")
      <*> strOption (long "node-socket" <> help "Cardano node socket")
      <*> option auto (long "port" <> help "Port of the webserver" <> value 8098)
