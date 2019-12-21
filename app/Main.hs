module Main where

import CLI
import ListPage
import ImagePage
-- import qualified Data.ByteString.Lazy.Char8 as Char8
import HTTPRequests
import System.Environment

-- TODO: Many tags
-- TODO: Command line params
-- TODO: Multithreaded downloading
-- TODO: Get all images from all image pages
-- TODO: More safety checks

main :: IO ()
main = do
  cliArguments <- getArgs
  imagePageURLs <- processListPage (listBaseURL ++ "fox_girl")
  downloadLink <- processImagePage (head imagePageURLs)
  downloadRaw downloadLink
