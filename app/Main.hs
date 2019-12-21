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

main :: IO ()
main = do
  _cliArguments <- getArgs
  cliArguments <- if null _cliArguments
    then printUsage >> errorWithoutStackTrace "No arguments passed"
    else getArgs

  if isSinglePage $ getTags cliArguments
    then do -- Download single page
      downloadLink <- processImagePage $ getTags cliArguments
      downloadRaw downloadLink
    else do -- Download all from page
      let tagString = makeTagURLPart $ getTags cliArguments
      downloadLink <- mapM processImagePage =<< processListPage (listBaseURL ++ tagString)
      parallel_ (map downloadRaw downloadLink) >> stopGlobalPool
