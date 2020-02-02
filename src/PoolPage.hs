module PoolPage (getPoolImageURLs) where

import qualified Data.ByteString.Lazy.Char8 as Char8 (unpack)
import HTTPRequests
import Data.Char
import qualified CLI
import Utility

isThumbnailLine :: String -> Bool
isThumbnailLine = go
  where
  go [] = False
  go ('i':'d':'=':'"':'p':_) = True
  go (_:cs) = go cs

extractID :: String -> String
extractID = filterOut (not . isDigit)

getImageIDs :: [String] -> [String]
getImageIDs html = map extractID (filter isThumbnailLine html)

baseImageURL :: String
baseImageURL = "https://gelbooru.com/index.php?page=post&s=view&id="

getPoolImageURLs :: String -> CLI.Count -> IO [String]
getPoolImageURLs url imgLimit = do
  let maxImg = case imgLimit of
        CLI.Unlimited -> defaultMaxImages
        CLI.Limited x -> x

  poolPageContents <- getPageContentsWget url

  let imageIDs = take maxImg $ getImageIDs $ lines $ Char8.unpack poolPageContents
  let urlBases = replicate (length imageIDs) baseImageURL

  let urls = map (\(x, y) -> x ++ y) $ zip urlBases imageIDs

  return urls
