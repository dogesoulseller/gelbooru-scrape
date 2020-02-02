module Main where

import qualified CLI
import ListPage
import ImagePage
import PoolPage
import HTTPRequests
import Utility
import URLFile

import System.Environment (getArgs)
import Control.Concurrent.ParallelIO.Global
import Control.Monad (when)
import System.Exit

-- TODO: More safety checks
-- TODO: Allow limiting by page count instead of image count

downloadInParallel :: [String] -> String -> IO ()
downloadInParallel downloadLinks outputDir =
  parallelInterleaved (map (downloadRaw outputDir) downloadLinks) >> stopGlobalPool >> exitSuccess

main :: IO ()
main = do
  -- Get command line arguments
  cliArguments <- getArgs
  when (null cliArguments) (CLI.printUsage >> errorWithoutStackTrace "No arguments passed")

  -- Check if all arguments are viable, error if they are not
  when (fst $ CLI.hasUnknownSettings cliArguments)
    $ CLI.printUsage >> errorWithoutStackTrace ("Unknown option " ++ snd (CLI.hasUnknownSettings cliArguments))

  -- Check if help message was requested
  when (CLI.requestsHelp cliArguments) (CLI.printUsage >> exitSuccess)

  -- Output directory
  outputDir <- CLI.getOutputDirectory cliArguments >>= \d -> return $ d ++ "/"

  -- URL list file
  let urlListFromFile = CLI.getInputFile cliArguments

  if not $ null urlListFromFile  -- Process from file
  then do
    pages <- loadURLFile urlListFromFile

    downloadLinks <- parallelInterleaved $ map processImagePage pages

    -- Download images
    downloadInParallel downloadLinks outputDir
  else if CLI.isImagePage $ last cliArguments -- Process single page
  then
    downloadRaw outputDir =<< processImagePage (last cliArguments)
  else if CLI.isPoolPage $ last cliArguments -- Process pool page
  then do
    let maxImages = CLI.getImgCount cliArguments
    imagePages <- getPoolImageURLs (last cliArguments) maxImages

    downloadLinks <- parallelInterleaved $ map processImagePage imagePages

    downloadInParallel downloadLinks outputDir
  else do -- Process from tags
    let maxImages = CLI.getImgCount cliArguments
    let tagString = makeTagURLPart $ last cliArguments

    -- Parse image pages
    imagePages <- processListPage (listBaseURL ++ tagString) maxImages
      >>= \ip -> return $ filterOut (== "https:>") ip

    when (null imagePages) (errorWithoutStackTrace "No posts found")

    -- Download images
    downloadLinks <- parallelInterleaved $ map processImagePage imagePages
    downloadInParallel downloadLinks outputDir