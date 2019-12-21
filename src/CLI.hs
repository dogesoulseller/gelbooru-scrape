module CLI (printUsage, getTags, isSinglePage) where

import Data.List (isPrefixOf)

printUsage :: IO ()
printUsage = mapM_ putStr ["\nUsage:\n"
  , "gelbooru-scrape [options] tags\n"
  , "Tags are a comma-separated list of tags to include in the search\n"
  , "To exclude a tag, prefix it with '-', eg. blue_hair,-long_hair\n"
  , "Options:\n"
  , "    None yet\n"]

getTags :: [String] -> String
getTags = last

isSinglePage :: String -> Bool
isSinglePage s = ("https://" `isPrefixOf` s || "http://" `isPrefixOf` s) && isImgURL s
  where
  isImgURL [] = False
  isImgURL ('&':'s':'=':'v':'i':'e':'w':_) = True
  isImgURL (_:xs) = isImgURL xs