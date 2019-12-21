module Main where

import CLI
import ListPage
import ImagePage
-- import qualified Data.ByteString.Lazy.Char8 as Char8
import HTTPRequests
import System.Environment
import Control.Concurrent.ParallelIO.Global

-- TODO: Get all images from all image pages
-- TODO: Extract set number of pages or images
-- TODO: More safety checks
-- TODO: Extract using tags or only single page

main :: IO ()
main = do
  _cliArguments <- getArgs
  cliArguments <- if null _cliArguments
    then printUsage >> errorWithoutStackTrace "No arguments passed"
    else getArgs
  let tagString = makeTagURLPart $ getTags cliArguments
  imagePageURLs <- processListPage (listBaseURL ++ tagString)
  downloadLink <- mapM processImagePage imagePageURLs
  parallel_ (map downloadRaw downloadLink) >> stopGlobalPool
