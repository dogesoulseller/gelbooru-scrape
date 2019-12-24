module ImagePage (processImagePage) where

import HTTPRequests

import qualified Data.ByteString.Lazy.Char8 as Char8 (unpack)

hasImageAnchor :: String -> Bool
hasImageAnchor [] = False
hasImageAnchor ('O':'r':'i':'g':'i':'n':'a':'l':' ':'i':'m':'a':'g':'e':_) = True
hasImageAnchor (_:cs) = hasImageAnchor cs

extractDirectLink :: String -> String
extractDirectLink line = takeWhile (/= '"') $ drop (hrefPos line 0) line
  where
  hrefPos [] acc = acc
  hrefPos (c:cs) acc
    | c == 'h' && take 13 cs == "ref=\"https://" = acc + 6
    | otherwise = hrefPos cs (acc+1)

getDownloadLink :: [String] -> String
getDownloadLink s = extractDirectLink . head $ dropWhile (not . hasImageAnchor) s

processImagePage :: String -> IO String
processImagePage url = do
  pageContents <- getPageContentsWget url
  return $ getDownloadLink (lines . Char8.unpack $ pageContents)
