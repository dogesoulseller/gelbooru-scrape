module ImagePage (processImagePage) where

import Utility
import HTTPRequests

import qualified Data.ByteString.Lazy.Char8 as Char8

hasImageAnchor :: String -> Bool
hasImageAnchor = go
  where
  go [] = False
  go (c:cs)
    | c == 'O' && take 13 cs == "riginal image" = True
    | otherwise = go cs

extractDirectLink :: String -> String
extractDirectLink line = takeWhile (/= '"') $ drop (findHref line 0) line
  where
  findHref [] acc = acc
  findHref (c:cs) acc
    | c == 'h' && take 13 cs == "ref=\"https://" = acc + 6
    | otherwise = findHref cs (acc+1)

getDownloadLink :: [String] -> String
getDownloadLink s = extractDirectLink $ head $ dropWhile (not . hasImageAnchor) s

processImagePage :: String -> IO String
processImagePage url = do
  pageContents <- getPageContentsWget url
  let pageContentsLines = lines . Char8.unpack $ pageContents
  return $ getDownloadLink pageContentsLines
  -- return $ Char8.unpack pageContents
