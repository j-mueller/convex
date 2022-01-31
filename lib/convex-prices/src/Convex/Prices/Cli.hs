{-# LANGUAGE NamedFieldPuns #-}
module Convex.Prices.Cli(
  runMain
  ) where

import Cardano.Api qualified as CAPI
import Control.Concurrent (forkIO)
import Control.Concurrent.STM qualified as STM
import Control.Monad (void)
import Control.Monad.Trans.Except (runExceptT)
import Convex.Muesli.Constants qualified as Constants
import Convex.NodeClient qualified as NC
import Convex.Prices.AssetPrices (emptyAssetPrices)
import Convex.Prices.Config (Config (..), configParser)
import Convex.Prices.NodeClient qualified as NC
import Convex.Prices.Webserver (ServerArgs (..), startServer)
import Data.Text qualified as Text
import Options.Applicative (customExecParser, disambiguate, helper, idm, info, prefs, showHelpOnEmpty, showHelpOnError)

runMain :: IO ()
runMain = do
  Config{cardanoNodeConfigFile, cardanoNodeSocket, webserverPort}  <- customExecParser
                (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                (info (helper <*> configParser) idm)
  let syncPoints = [Constants.muesliStart]
  svPrices <- STM.newTVarIO emptyAssetPrices
  void $ forkIO $ do
    putStrLn $ "Starting webserver on port " <> show webserverPort
    startServer ServerArgs{svPort = webserverPort, svPrices}
  let client _connectInfo env = do
        return (NC.resumingClient syncPoints (const $ NC.pricesClient svPrices env))
  result <- runExceptT (NC.runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> do
      putStrLn "Error in runNodeClient"
      putStrLn (Text.unpack $ CAPI.renderInitialLedgerStateError err)
    Right () -> do
      putStrLn "runNodeClient successful."

