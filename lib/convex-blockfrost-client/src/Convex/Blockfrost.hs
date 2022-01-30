{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| Bindings to blockfrost's @tx/submit@ endpoint
-}
module Convex.Blockfrost(
  BlockfrostProjectId(..),
  submitTxn
  ) where

import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wreq (header)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (Postable (..))

newtype BlockfrostProjectId = BlockfrostProjectId{getBlockfrostProjectId :: Text }

newtype CBORByteString = CBORByteString { getCBORByteString :: ByteString }

instance Postable CBORByteString where
  postPayload = payload "application/cbor" . HTTP.RequestBodyBS . getCBORByteString

{-| Send a CBOR serialised transaction to the network via blockfrost
-}
submitTxn :: BlockfrostProjectId -> ByteString -> IO (Either String ())
submitTxn BlockfrostProjectId{getBlockfrostProjectId} bs = fmap (join . first show) $ try @SomeException $ do
  let opts = Wreq.defaults & header "project_id" .~ [TE.encodeUtf8 getBlockfrostProjectId]
  r <- Wreq.postWith opts "https://cardano-mainnet.blockfrost.io/api/v0/tx/submit" (CBORByteString bs)
  let st = HTTP.responseStatus r
  if HTTP.statusCode st == 200
    then pure (Right ())
    else do
      putStrLn $ "Blockfrost.submitTxn: " <> show st
      pure (Left $ show st)

payload :: ByteString -> HTTP.RequestBody -> HTTP.Request -> IO HTTP.Request
payload ct body req =
  let hds = HTTP.requestHeaders req
      newHeaders = hds ++ [("Content-Type", ct)]
  in return $ req { HTTP.requestBody = body, HTTP.requestHeaders = newHeaders }
