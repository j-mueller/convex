{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
module Convex.Wallet.Cli(runMain) where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT)
import Convex.NodeClient.Types (loadConnectInfo)
import Convex.Wallet.Command (CliCommand (..))
import Convex.Wallet.Command qualified as Command
import Convex.Wallet.Config (Config (..), ConfigMode (..))
import Convex.Wallet.Config qualified as Config
import Convex.Wallet.NodeEnv (BalanceTxNodeEnv (..))
import Convex.Wallet.NodeEnv qualified as NodeEnv
import Convex.Wallet.Types (Wallet (..))
import Convex.Wallet.Types qualified as T
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative (customExecParser, disambiguate, helper, idm, info, prefs, showHelpOnEmpty, showHelpOnError)
import System.Exit (exitFailure)

runMain :: IO ()
runMain = do
  command <- customExecParser
                (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                (info (helper <*> Command.commandParser) idm)
  result <- runExceptT $ do
    case command of
      GenerateKey          -> generateKey
      RunWallet config     -> getConfig config >>= runWallet
      ShowAddress config   -> getConfig config >>= showAddress
      ConnectToNode config -> getConfig config >>= connectToNode
  case result of
    Left err -> do
      putStrLn "Error in runNodeClient"
      putStrLn (Text.unpack $ C.renderInitialLedgerStateError err)
    Right () -> do
      putStrLn "runNodeClient successful."

generateKey :: MonadIO m => m ()
generateKey = liftIO $ do
  putStrLn "Generating key..."
  key <- C.generateSigningKey C.AsPaymentKey
  Text.putStrLn (C.serialiseToBech32 key)

runWallet :: MonadIO m => Config 'Typed -> m ()
runWallet _ =
  pure ()

showAddress :: (MonadError C.InitialLedgerStateError m, MonadIO m) => Config 'Typed -> m ()
showAddress Config{nonAdaReturnAddress, walletKey, cardanoNodeConfigFile, cardanoNodeSocket} = do
  liftIO $ putStr "Return address: "
  liftIO $ Text.putStrLn (C.serialiseToBech32 nonAdaReturnAddress)
  liftIO $ putStr "Wallet key:     "
  liftIO $ Text.putStrLn (C.serialiseToBech32 walletKey)
  (C.LocalNodeConnectInfo{C.localNodeNetworkId}, _) <- loadConnectInfo cardanoNodeConfigFile cardanoNodeSocket
  let wllt = Wallet{wKey = walletKey, wNonAdaReturnAddress = nonAdaReturnAddress}
  liftIO $ putStr "Wallet address: "
  liftIO $ Text.putStrLn (C.serialiseToBech32 $ T.address localNodeNetworkId wllt)

getConfig :: MonadIO m => Config 'Str -> m (Config 'Typed)
getConfig c = case Config.mkTyped c of
  Left err -> liftIO $ do
    putStrLn (show err)
    exitFailure
  Right k -> pure k

connectToNode :: (MonadError C.InitialLedgerStateError m, MonadIO m) => Config 'Typed -> m ()
connectToNode Config{cardanoNodeConfigFile, cardanoNodeSocket} = do
  (connectInfo, _) <- loadConnectInfo cardanoNodeConfigFile cardanoNodeSocket
  result <- liftIO (NodeEnv.getNodeEnv connectInfo)
  case result of
    Left err -> liftIO $ do
      putStrLn (show err)
      exitFailure
    Right BalanceTxNodeEnv{bteEra, bteSystemStart, bteActivePools, bteNetworkId} -> liftIO $ do
      putStrLn $ "Era:               " <> show bteEra
      putStrLn $ "System start:      " <> show bteSystemStart
      putStrLn $ "# of active pools: " <> show (Set.size bteActivePools)
      putStrLn $ "Network ID:        " <> show bteNetworkId
