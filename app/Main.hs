module Main where

import CLI
import ListPage
import ImagePage
-- import qualified Data.ByteString.Lazy.Char8 as Char8
import HTTPRequests
import System.Environment
import Control.Concurrent.ParallelIO.Global
import Utility
import Control.Monad

-- TODO: Get all images from all image pages
-- TODO: More safety checks

main :: IO ()
main = do
  -- Get command line arguments
  _cliArguments <- getArgs
  cliArguments <- if null _cliArguments
    then printUsage >> errorWithoutStackTrace "No arguments passed"
    else getArgs

  -- Check if all arguments are viable, error if they are not
  when (fst $ hasUnknownSettings cliArguments)
    $ printUsage >> errorWithoutStackTrace ("Unknown option " ++ snd (hasUnknownSettings cliArguments))

  -- Process
  if isSinglePage $ last cliArguments
    then do -- Download single page
      downloadLink <- processImagePage $ last cliArguments
      downloadRaw downloadLink
    else do -- Download all from page
      let maxImages = CLI.getImgCount cliArguments
      let tagString = makeTagURLPart $ last cliArguments

      _imagePages <- processListPage (listBaseURL ++ tagString)
      let _imagePageF = filterOut (== "https:>") _imagePages
      let imagePages = if null _imagePageF
          then errorWithoutStackTrace "No posts found"
          else case maxImages of
            CLI.Unlimited -> _imagePageF
            CLI.Limited limit -> take limit _imagePageF
      downloadLink <- mapM processImagePage imagePages
      parallel_ (map downloadRaw downloadLink) >> stopGlobalPool