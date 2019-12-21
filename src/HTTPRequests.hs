module HTTPRequests (getPageContentsWget, downloadRaw) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import System.Process

getPageContentsWget :: String -> IO Char8.ByteString
getPageContentsWget url =
  do
    (_, Just hstdout, _, hproc) <- createProcess (proc "wget" ["-O-", url]){ std_out = CreatePipe }
    Char8.hGetContents hstdout;

downloadRaw :: String -> IO ()
downloadRaw url =
  callProcess "wget" [url]
