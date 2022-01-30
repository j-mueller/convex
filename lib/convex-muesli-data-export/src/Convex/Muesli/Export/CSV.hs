{-# LANGUAGE OverloadedStrings #-}
module Convex.Muesli.Export.CSV(
  ToRow(..),
  appendRow,
  writeHeaders
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Handle (Handle)

class ToRow a where
  header :: Proxy a -> [Text]
  row :: a -> [Text]

appendRow :: ToRow a => Handle -> a -> IO ()
appendRow handle vl =
  let rw = T.intercalate ";" (row vl)
  in T.hPutStrLn handle rw

writeHeaders :: ToRow a => Proxy a -> Handle -> IO ()
writeHeaders p handle =
  let rw = T.intercalate ";" (header p)
  in T.hPutStrLn handle rw
