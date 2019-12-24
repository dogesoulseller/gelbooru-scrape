module Main where

import qualified CLI
import ListPage
import ImagePage
import HTTPRequests
import Utility

import System.Environment (getArgs)
import Control.Concurrent.ParallelIO.Global (parallel_, stopGlobalPool)
import Control.Monad (when)

-- TODO: More safety checks
-- TODO: Allow limiting by page count instead of image count

main :: IO ()
main = do
  -- Get command line arguments
  cliArguments <- getArgs
  when (null cliArguments) (errorWithoutStackTrace "No arguments passed")

  -- Check if all arguments are viable, error if they are not
  when (fst $ CLI.hasUnknownSettings cliArguments)
    $ CLI.printUsage >> errorWithoutStackTrace ("Unknown option " ++ snd (CLI.hasUnknownSettings cliArguments))

  outputDir <- CLI.getOutputDirectory cliArguments >>= \d -> return $ d ++ "/"

  -- Process
  if CLI.isSinglePage $ last cliArguments
  then downloadRaw outputDir =<< processImagePage (last cliArguments)
  else do
    let maxImages = CLI.getImgCount cliArguments
    let tagString = makeTagURLPart $ last cliArguments

    -- Parse image pages
    imagePages <- processListPage (listBaseURL ++ tagString) maxImages
      >>= \ip -> return $ filterOut (== "https:>") ip

    when (null imagePages) $ errorWithoutStackTrace "No posts found"

    -- Download images
    downloadLinks <- mapM processImagePage imagePages
    parallel_ (map (downloadRaw outputDir) downloadLinks) >> stopGlobalPool