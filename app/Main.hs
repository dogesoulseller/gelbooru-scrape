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

-- TODO: More safety checks
-- TODO: Allow limiting by page count instead of image count

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

  _outputDir <- getOutputDirectory cliArguments
  let outputDir = _outputDir ++ "/"

  -- Process
  if isSinglePage $ last cliArguments
    then -- Download single page
      (\url -> downloadRaw url outputDir) =<< processImagePage (last cliArguments)
    else do -- Download all from page
      let maxImages = CLI.getImgCount cliArguments
      let tagString = makeTagURLPart $ last cliArguments

      -- Parse image pages
      _imagePages <- processListPage (listBaseURL ++ tagString) maxImages
      let imagePages = filterOut (== "https:>") _imagePages
      when (null imagePages) $ errorWithoutStackTrace "No posts found"

      -- Download images
      downloadLink <- mapM processImagePage imagePages
      parallel_ (map (\url -> downloadRaw url outputDir) downloadLink) >> stopGlobalPool