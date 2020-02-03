module Main where

import qualified CLI
import ListPage
import ImagePage
import PoolPage
import HTTPRequests
import Utility
import URLFile

import System.Environment (getArgs)
import Control.Concurrent.ParallelIO.Local
import Control.Monad (when)
import System.Exit
import GHC.Conc (getNumCapabilities, setNumCapabilities)

-- TODO: More safety checks

getPoolThreadCount :: CLI.Count -> IO Int
getPoolThreadCount count = case count of
  CLI.Unlimited -> getNumCapabilities
  CLI.Limited x -> do
    setNumCapabilities x
    return x

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

  -- Maximum amount of images requested
  let maxImagesDirect = CLI.getImgCount cliArguments
  let maxImagesPage = CLI.getPageCount cliArguments
  let maxImages = if CLI.countIsLimited maxImagesDirect then maxImagesDirect -- Has max count specified via -c
      else if CLI.countIsLimited maxImagesPage && CLI.countIsUnlimited maxImagesDirect then maxImagesPage -- Has max count specified via -p
      else maxImagesDirect  -- Has max count specified via both or neither

  -- URL list file
  let urlListFromFile = CLI.getInputFile cliArguments

  -- Thread count
  requestedThreads <- getPoolThreadCount $ CLI.getThreadCount cliArguments

  withPool requestedThreads $ \threadPool ->

    if not $ null urlListFromFile then do -- Process from file
      pages <- loadURLFile urlListFromFile

      downloadLinks <- parallelInterleaved threadPool $ map processImagePage pages

      parallelInterleaved threadPool (map (downloadRaw outputDir) downloadLinks) >> exitSuccess
    else if CLI.isImagePage $ last cliArguments then -- Process single page
      downloadRaw outputDir =<< processImagePage (last cliArguments)
    else if CLI.isPoolPage $ last cliArguments then do -- Process pool page
      imagePages <- getPoolImageURLs (last cliArguments) maxImages

      downloadLinks <- parallelInterleaved threadPool $ map processImagePage imagePages

      parallelInterleaved threadPool (map (downloadRaw outputDir) downloadLinks) >> exitSuccess
    else do -- Process from tags
      let tagString = makeTagURLPart $ last cliArguments

      -- Parse image pages
      imagePages <- processListPage (listBaseURL ++ tagString) maxImages
        >>= \ip -> return $ filterOut (== "https:>") ip

      when (null imagePages) (errorWithoutStackTrace "No posts found")

      downloadLinks <- parallelInterleaved threadPool $ map processImagePage imagePages
      parallelInterleaved threadPool (map (downloadRaw outputDir) downloadLinks) >> exitSuccess