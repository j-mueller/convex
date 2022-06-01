{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-| Webserver for queries and txn submission
-}
module Convex.Wallet.Webserver(
  ServerArgs(..),
  startServer
  ) where

import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Wallet.Stats (Stats, WalletStats)
import Convex.Wallet.Utxos (UtxoState (..))
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp qualified
import Servant (Application, Get, JSON, Server, serve, (:>))

data ServerArgs =
  ServerArgs
    { svUtxo        :: TVar UtxoState
    , svAllStats    :: TVar Stats
    , svWalletStats :: TVar WalletStats
    , svPort        :: Int
    }

{-| Get the number of UTXOs managed by the wallet
-}
type API = "utxos" :> Get '[JSON] Int

server :: ServerArgs -> Server API
server ServerArgs{svUtxo} =
  liftIO $ atomically $ do
    UtxoState{_walletUtxos} <- readTVar svUtxo
    return $ Map.size _walletUtxos

app :: ServerArgs -> Application
app args = serve (Proxy @API) (server args)

startServer :: ServerArgs -> IO ()
startServer args@ServerArgs{svPort} =
  Network.Wai.Handler.Warp.run svPort (app args)
