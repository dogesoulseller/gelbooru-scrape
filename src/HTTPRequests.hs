module HTTPRequests (getPageContentsWget, downloadRaw) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Network.HTTP.Conduit
import System.IO
import Utility

getPageContentsWget :: String -> IO Char8.ByteString
getPageContentsWget = simpleHttp

downloadRaw :: String -> String -> IO ()
downloadRaw url outDir = withFile (outDir ++ lastURLComponent url) WriteMode $
  \outFile -> Char8.hPut outFile =<< simpleHttp url
