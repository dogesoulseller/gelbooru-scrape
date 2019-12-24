module HTTPRequests (getPageContentsWget, downloadRaw) where

import qualified Data.ByteString.Lazy.Char8 as Char8 (hPut, ByteString)
import Network.HTTP.Conduit (simpleHttp)
import System.IO (withFile, IOMode(WriteMode))

import Utility

getPageContentsWget :: String -> IO Char8.ByteString
getPageContentsWget = simpleHttp

downloadRaw :: String -> String -> IO ()
downloadRaw outDir url = withFile (outDir ++ lastURLComponent url) WriteMode $
  \file -> Char8.hPut file =<< simpleHttp url
