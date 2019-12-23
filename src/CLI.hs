module CLI (printUsage, isSinglePage, Count(Limited, Unlimited), getImgCount, hasUnknownSettings, getOutputDirectory) where

import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import System.Directory

printUsage :: IO ()
printUsage = mapM_ putStr ["\nUsage:\n"
  , "gelbooru-scrape [options] tags\n"
  , "Tags are a comma-separated list of tags to include in the search\n"
  , "To exclude a tag, prefix it with '-', eg. blue_hair,-long_hair\n"
  , "Options:\n"
  , "    -c = maximum count of images to get\n"
  , "    -o = output directory [default: working dir]"]

isSinglePage :: String -> Bool
isSinglePage s = ("https://" `isPrefixOf` s || "http://" `isPrefixOf` s) && isImgURL s
  where
  isImgURL [] = False
  isImgURL ('&':'s':'=':'v':'i':'e':'w':_) = True
  isImgURL (_:xs) = isImgURL xs

data Count = Limited Int | Unlimited

getImgCount :: [String] -> Count
getImgCount args = if findImgOption args == 0 then Unlimited else Limited $ findImgOption args
  where
  findImgOption :: [String] -> Int
  findImgOption [] = 0
  findImgOption (o:os)
    | o == "-c" = if null os
      then errorWithoutStackTrace "Image count option was without value"
      else case readMaybe (head os) of
        Nothing -> errorWithoutStackTrace "Cannot convert image count value to integer"
        Just x -> x
    | otherwise = findImgOption os

getOutputDirectory :: [String] -> IO String
getOutputDirectory args = if null $ findDirOption args then getCurrentDirectory else return $ findDirOption args
  where
  findDirOption [] = []
  findDirOption (o:os)
    | o == "-o" = if null os then errorWithoutStackTrace "Dir output was without value" else head os
    | otherwise = findDirOption os

hasUnknownSettings :: [String] -> (Bool, String)
hasUnknownSettings args = go $ take (length args - 1) args
  where
  go [] = (False, "")
  go (o:os)
    | o == "-c" = go os -- Max image count
    | o == "-o" = go os -- Output dir
    | "-" `isPrefixOf` o = (True, o) -- Is option, but is not recognized
    | otherwise = go os