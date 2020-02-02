module ListPage (processListPage, listBaseURL, makeTagURLPart) where

import qualified Data.ByteString.Lazy.Char8 as Char8 (unpack)

import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

import qualified CLI
import HTTPRequests
import Utility

listBaseURL :: String
listBaseURL = "https://gelbooru.com/index.php?page=post&s=list&tags="

isThumbPreview :: Char -> String -> Bool
isThumbPreview c s = c == '<' && take 30 s == "div class=\"thumbnail-preview\">"

firstThumbnail :: String -> Maybe Int
firstThumbnail s = if go s 0 == -1 then Nothing else Just $ go s 0
  where
  go [] _ = -1 :: Int
  go (c:cs) pos
    | isThumbPreview c cs = pos
    | otherwise = go cs pos+1

lastThumbnail :: String -> Int
lastThumbnail s = findEnd (drop startOfLast s) startOfLast
  where
  go [] _ final = final
  go (c:cs) pos final
    | isThumbPreview c cs = go cs (pos+1) (pos+1)
    | otherwise = go cs (pos+1) final
  findEnd [] pos = pos
  findEnd ('<':'/':'a':'>':'<':'/':'s':'p':'a':'n':'>':_) pos = pos
  findEnd (_:cs) pos = findEnd cs (pos+1)

  startOfLast :: Int
  startOfLast = go s 0 0

processListPage :: String -> CLI.Count -> IO [String]
processListPage url imgLimit = do
  let maxImg = case imgLimit of
        CLI.Unlimited -> defaultMaxImages
        CLI.Limited x -> x
  let pagesToGet = maxImg `div` 42 + if maxImg `mod` 42 /= 0 then 1 else 0
  let pages = map (\(x, y, z) -> x ++ y ++ show z) $ zip3 (replicate pagesToGet url) (replicate pagesToGet "&pid=") (pids pagesToGet)
  results <- mapM getPageContentsWget pages
  return $ take maxImg $ map (("https:" ++) . pageFromThumbnail) $ concatMap (trimHTML . Char8.unpack) results

  where
  pageFromThumbnail s = replaceSpecAmp . takeWhile (/= '"') $ drop (hrefPos s) s
  trimHTML s = filterOut ("</div>" `isPrefixOf`) $ map (dropWhile isSpace) (lines $ posts s)
  pids n = take n [0,42..] :: [Int]
  posts s = slice firstDiv (firstDiv+lastDiv) s
    where
    firstDiv = fromMaybe (error "No posts found") (firstThumbnail s)
    lastDiv = lastThumbnail $ drop firstDiv s
  hrefPos s = go s 0
    where
    go [] _ = -1
    go ('h':'r':'e':'f':'=':'"':_) pos = pos + 6
    go (_:cs) pos = go cs (pos + 1)

makeTagURLPart :: String -> String
makeTagURLPart = replaceSeps . replaceSpecialChars
  where
  replaceSeps [] = []
  replaceSeps (',':xs) = '+' : replaceSeps xs
  replaceSeps (' ':xs) = '_' : replaceSeps xs
  replaceSeps (x:xs) = x : replaceSeps xs