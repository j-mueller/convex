{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module Convex.Muesli.Export.Cli(runMain) where

import Cardano.Api qualified as CAPI
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Convex.Muesli.Constants qualified as Constants
import Convex.Muesli.Export.CSV qualified as CSV
import Convex.Muesli.Export.Config (Config (..), configParser)
import Convex.Muesli.Export.NodeClient qualified as NC
import Convex.Muesli.Transaction (Transaction)
import Convex.NodeClient qualified as NC
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Options.Applicative (customExecParser, disambiguate, helper, idm, info, prefs, showHelpOnEmpty, showHelpOnError)
import System.IO (IOMode (WriteMode), withFile)

runMain :: IO ()
runMain = do
  Config{cardanoNodeConfigFile, cardanoNodeSocket, csvFilePath, ownKeys}  <- customExecParser
                (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                (info (helper <*> configParser) idm)
  when (null ownKeys) $ putStrLn "WARNING: No staking keys. 'matchmaker' will always be 'other'"
  putStrLn $ "Writing CSV to " <> csvFilePath
  withFile csvFilePath WriteMode $ \handle -> do
    let syncPoints = [Constants.muesliStart]
        client _connectInfo env = do
          CSV.writeHeaders (Proxy @Transaction) handle
          return $ NC.resumingClient syncPoints (const $ NC.exportClient (Set.fromList ownKeys) handle env)
    result <- runExceptT (NC.runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
    case result of
      Left err -> do
        putStrLn "Error in runNodeClient"
        putStrLn (Text.unpack $ CAPI.renderInitialLedgerStateError err)
      Right () -> do
        putStrLn "runNodeClient successful."
