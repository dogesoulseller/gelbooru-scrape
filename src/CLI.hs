module CLI (printUsage, isImagePage, isPoolPage, Count(Limited, Unlimited), getImgCount, hasUnknownSettings, getOutputDirectory,
  requestsHelp, getInputFile) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Directory (getCurrentDirectory)

data Count = Limited Int | Unlimited

printUsage :: IO ()
printUsage = mapM_ putStr ["\nUsage:\n"
  , "gelbooru-scrape [options] tags/URL\n"
  , "Tags are a comma-separated list of tags to include in the search\n"
  , "To exclude a tag, prefix it with '-', eg. blue_hair,-long_hair\n"
  , "URL is a single URL for either a single page or a results page\n"
  , "Options:\n"
  , "    -h = show this help message\n"
  , "    -f = path to file containing newline-separated URLs to download\n"
  , "    -c = maximum count of images to get\n"
  , "    -o = output directory [default: working dir]\n"]

isPoolPage :: String -> Bool
isPoolPage s = ("https://" `isPrefixOf` s || "http://" `isPrefixOf` s) && isPoolURL s
  where
  isPoolURL [] = False
  isPoolURL ('p':'a':'g':'e':'=':'p':'o':'o':'l':_) = True
  isPoolURL (_:xs) = isPoolURL xs

isImagePage :: String -> Bool
isImagePage s = ("https://" `isPrefixOf` s || "http://" `isPrefixOf` s) && isImgURL s
  where
  isImgURL [] = False
  isImgURL ('&':'s':'=':'v':'i':'e':'w':_) = True
  isImgURL (_:xs) = isImgURL xs

getImgCount :: [String] -> Count
getImgCount args = if findImgOption args == 0 then Unlimited else Limited $ findImgOption args
  where
  findImgOption :: [String] -> Int
  findImgOption [] = 0
  findImgOption ("-c":os) = if null os then
    errorWithoutStackTrace "Image count option was without value" else
      fromMaybe (errorWithoutStackTrace "Cannot convert image count value to integer") (readMaybe (head os))
  findImgOption (_:os) = findImgOption os

getOutputDirectory :: [String] -> IO String
getOutputDirectory args = if null $ findDirOption args then getCurrentDirectory else return $ findDirOption args
  where
  findDirOption [] = []
  findDirOption ("-o":os) = if null os then errorWithoutStackTrace "Dir output was without value" else head os
  findDirOption (_:os) = findDirOption os

getInputFile :: [String] -> String
getInputFile args = if null $ findInputFileOption args then [] else findInputFileOption args
  where
  findInputFileOption [] = []
  findInputFileOption ("-f":os) = if null os then errorWithoutStackTrace "URL file option was given no value" else head os
  findInputFileOption (_:os) = findInputFileOption os

requestsHelp :: [String] -> Bool
requestsHelp [] = False
requestsHelp ("-h":_) = True
requestsHelp (_:os) = requestsHelp os

hasUnknownSettings :: [String] -> (Bool, String)
hasUnknownSettings args = go $ take (length args - 1) args
  where
  go [] = (False, "")
  go (o:os)
    | o == "-c" = go os -- Max image count
    | o == "-o" = go os -- Output dir
    | o == "-h" = go os -- Show help
    | o == "-f" = go os -- Output file
    | "-" `isPrefixOf` o = (True, o) -- Is option, but is not recognized
    | otherwise = go os
