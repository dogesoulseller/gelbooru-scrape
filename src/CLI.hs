module CLI (printUsage, getTags) where

import System.Environment
import Data.List

printUsage :: IO ()
printUsage = mapM_ putStr ["\nUsage:\n"
  , "gelbooru-scrape [options] tags\n"
  , "Tags are a comma-separated list of tags to include in the search\n"
  , "To exclude a tag, prefix it with '-', eg. blue_hair,-long_hair\n"]

getTags :: [String] -> Maybe String
getTags s = if null s then Nothing else Just (last s)
