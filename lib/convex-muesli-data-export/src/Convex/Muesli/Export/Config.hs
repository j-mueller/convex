module Convex.Muesli.Export.Config(
  Config(..),
  configParser
  ) where

import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Crypto qualified as C
import Convex.Muesli.Constants (getStakingKey)
import Options.Applicative (Parser, help, long, strOption, option, maybeReader)
import Options.Applicative.Types (fromM, manyM)
import qualified Data.Text as Text

data Config =
  Config
    { cardanoNodeConfigFile :: FilePath
    , cardanoNodeSocket     :: FilePath
    , csvFilePath           :: FilePath
    , ownKeys               :: [L.StakeReference C.StandardCrypto]
    }

configParser :: Parser Config
configParser =
  Config
    <$> strOption (long "node-config" <> help "Cardano node config JSON file")
    <*> strOption (long "node-socket" <> help "Cardano node socket")
    <*> strOption (long "csv" <> help "CSV file (will be overwritten)")
    <*> fromM (manyM stakingCredentialParser)

stakingCredentialParser :: Parser (L.StakeReference C.StandardCrypto)
stakingCredentialParser =
  option
    (maybeReader (getStakingKey . Text.pack))
    ( long "own-address"
    <> help "An address that we consider to be one of ours. This is used for determining the match maker market share."
    )
