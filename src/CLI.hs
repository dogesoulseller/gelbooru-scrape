module CLI (printUsage, isImagePage, isPoolPage,
  Count(Limited, Unlimited), countIsLimited, countIsUnlimited,
  getImgCount, getPageCount, getThreadCount, hasUnknownSettings, getOutputDirectory,
  requestsHelp, getInputFile) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Directory (getCurrentDirectory)

data Count = Limited Int | Unlimited
countIsLimited :: Count -> Bool
countIsLimited (Limited _) = True
countIsLimited _ = False
countIsUnlimited :: Count -> Bool
countIsUnlimited Unlimited = True
countIsUnlimited _ = False

printUsage :: IO ()
printUsage = mapM_ putStr ["\nUsage:\n"
  , "gelbooru-scrape [options] tags/URL\n"
  , "Tags are a comma-separated list of tags (without spaces) to include in the search\n"
  , "To exclude a tag, prefix it with '-', eg. blue_hair,-long_hair\n"
  , "URL is a single URL for either a single page or a results page\n"
  , "Note: URLs and tags should be placed in quotes to avoid weird behavior\n"
  , "Options:\n"
  , "    -h = show this help message\n"
  , "    -t = thread count [defaults to present logical cores]\n"
  , "    -f = path to file containing newline-separated URLs to download\n"
  , "    -c = maximum count of images to get\n"
  , "    -p = maximum count of pages to get (if both -c and -p are specified, -c takes precedence)\n"
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

getThreadCount :: [String] -> Count
getThreadCount args = if findThreadOption args == 0 then Unlimited else Limited $ findThreadOption args
  where
  findThreadOption :: [String] -> Int
  findThreadOption [] = 0
  findThreadOption ("-t":os) = if null os then
    errorWithoutStackTrace "Thread count option was without value" else
      fromMaybe (errorWithoutStackTrace "Cannot convert thread count value to integer") (readMaybe (head os))
  findThreadOption (_:os) = findThreadOption os

getImgCount :: [String] -> Count
getImgCount args = if findImgOption args == 0 then Unlimited else Limited $ findImgOption args
  where
  findImgOption :: [String] -> Int
  findImgOption [] = 0
  findImgOption ("-c":os) = if null os then
    errorWithoutStackTrace "Image count option was without value" else
      fromMaybe (errorWithoutStackTrace "Cannot convert image count value to integer") (readMaybe (head os))
  findImgOption (_:os) = findImgOption os

getPageCount :: [String] -> Count
getPageCount args = if findPagesOption args == 0 then Unlimited else Limited $ (findPagesOption args) * 42
  where
  findPagesOption :: [String] -> Int
  findPagesOption [] = 0
  findPagesOption ("-p":os) = if null os then
    errorWithoutStackTrace "Page image count option was without value" else
      fromMaybe (errorWithoutStackTrace "Cannot convert page image count value to integer") (readMaybe (head os))
  findPagesOption (_:os) = findPagesOption os

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
    | o == "-p" = go os -- Max page count
    | o == "-t" = go os -- Thread count
    | o == "-o" = go os -- Output dir
    | o == "-h" = go os -- Show help
    | o == "-f" = go os -- Output file
    | "-" `isPrefixOf` o = (True, o) -- Is option, but is not recognized
    | otherwise = go os
