module ListPage (processListPage, listBaseURL, makeTagURLPart) where
-- Find highest PID

import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as Char8
import HTTPRequests
import Utility
import qualified CLI

listBaseURL :: String
listBaseURL = "https://gelbooru.com/index.php?page=post&s=list&tags="

defaultMaxImages :: Int
defaultMaxImages = 500

findHrefVal :: String -> Int
findHrefVal s = go s 0
  where
  go [] _ = -1
  go (c:cs) pos
    | c == 'h' && take 5 cs == "ref=\"" = pos+6
    | otherwise = go cs (pos+1)

getFirstThumbnail :: String -> Maybe Int
getFirstThumbnail s = if out == -1 then Nothing else Just out
  where
  out = go s 0
  go [] _ = -1
  go (c:cs) pos
    | c == '<' && take 30 cs == "div class=\"thumbnail-preview\">" = pos
    | otherwise = go cs pos+1

getLastThumbnail :: String -> Int
getLastThumbnail s = findEnd (drop startOfLast s) startOfLast
  where
  go [] _ final = final
  go (c:cs) pos final
    | c == '<' && take 30 cs == "div class=\"thumbnail-preview\">" = go cs (pos+1) (pos+1)
    | otherwise = go cs (pos+1) final

  findEnd [] pos = pos
  findEnd (c:cs) pos
    | c == '<' && take 10 cs == "/a></span>" = pos
    | otherwise = findEnd cs (pos+1)

  startOfLast = go s 0 0

getPosts :: String -> String
getPosts s = slice firstDiv (firstDiv+lastDiv) s
  where
    firstDiv = case getFirstThumbnail s of
      Nothing -> error "No posts found"
      Just x -> x
    lastDiv = getLastThumbnail $ drop firstDiv s

trimHTML :: String -> [String]
trimHTML s = filterOut ("</div>" `isPrefixOf`) $ map (dropWhile isSpace) (lines $ getPosts s)

extractPageFromThumbnail :: String -> String
extractPageFromThumbnail s = replaceSpecAmp . takeWhile (/= '"') $ drop (findHrefVal s) s

processListPage :: String -> CLI.Count -> IO [String]
processListPage url imgLimit = do
  let maxImg = case imgLimit of
        CLI.Unlimited -> defaultMaxImages
        CLI.Limited x -> x
  let pagesToGet = maxImg `div` 42 + if maxImg `mod` 42 /= 0 then 1 else 0
  let pages = map (\(x, y, z) -> x ++ y ++ show z) $ zip3 (replicate pagesToGet url) (replicate pagesToGet "&pid=") (pids pagesToGet)
  results <- mapM getPageContentsWget pages
  return $ take maxImg $ map (("https:" ++) . extractPageFromThumbnail) $ concatMap (trimHTML . Char8.unpack) results

  where
    pids :: Int -> [Int]
    pids n = take n [x*42 | x <- [0..]]

makeTagURLPart :: String -> String
makeTagURLPart = replaceSeps . replaceSpecialChars
  where
  replaceSeps [] = []
  replaceSeps (',':xs) = '+' : replaceSeps xs
  replaceSeps (' ':xs) = '_' : replaceSeps xs
  replaceSeps (x:xs) = x : replaceSeps xs