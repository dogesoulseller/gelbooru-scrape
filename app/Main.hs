module Main where

import CLI
import ListPage
import ImagePage
-- import qualified Data.ByteString.Lazy.Char8 as Char8
import HTTPRequests
import System.Environment
import Control.Concurrent.ParallelIO.Global
import Utility

-- TODO: Get all images from all image pages
-- TODO: Extract set number of pages or images
-- TODO: More safety checks

main :: IO ()
main = do
  _cliArguments <- getArgs
  cliArguments <- if null _cliArguments
    then printUsage >> errorWithoutStackTrace "No arguments passed"
    else getArgs

  if isSinglePage $ last cliArguments
    then do -- Download single page
      downloadLink <- processImagePage $ last cliArguments
      downloadRaw downloadLink
    else do -- Download all from page
      let tagString = makeTagURLPart $ last cliArguments
      _imagePages <- processListPage (listBaseURL ++ tagString)
      let imagePages = if null $ filterOut (== "https:>") _imagePages
          then errorWithoutStackTrace "No posts found"
          else filterOut (== "https:>") _imagePages
      downloadLink <- mapM processImagePage imagePages
      parallel_ (map downloadRaw downloadLink) >> stopGlobalPool
