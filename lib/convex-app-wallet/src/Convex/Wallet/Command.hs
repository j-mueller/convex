{-# LANGUAGE DataKinds #-}
module Convex.Wallet.Command(CliCommand(..), commandParser) where

import Convex.Wallet.Config (Config, ConfigMode (..), configParser)
import Options.Applicative (CommandFields, Mod, Parser, command, fullDesc, info, progDesc, subparser)

data CliCommand =
  GenerateKey
  | RunWallet (Config 'Str)
  | ShowAddress (Config 'Str)
  | ConnectToNode (Config 'Str)
  deriving (Eq, Ord, Show)

commandParser :: Parser CliCommand
commandParser =
  subparser $
    mconcat
      [ generateKey
      , runWallet
      , showAddress
      , connectToNode
      ]

generateKey :: Mod CommandFields CliCommand
generateKey = command "generate-key" $
  info (pure GenerateKey) (fullDesc <> progDesc "Generate a wallet key")

runWallet :: Mod CommandFields CliCommand
runWallet = command "run-wallet" $
  info (RunWallet <$> configParser) (fullDesc <> progDesc "Start the wallet")

showAddress :: Mod CommandFields CliCommand
showAddress = command "show-address" $
  info (ShowAddress <$> configParser) (fullDesc <> progDesc "Show the address of the wallet")

connectToNode :: Mod CommandFields CliCommand
connectToNode = command "node-connect" $
  info (ConnectToNode <$> configParser) (fullDesc <> progDesc "Connect to the local cardano node and print some debug information")
