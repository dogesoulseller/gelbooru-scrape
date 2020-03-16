module Utility (defaultMaxImages, imagesPerPage, slice, filterOut, replaceSpecAmp, replaceSpecialChars, lastURLComponent) where

defaultMaxImages :: Int
defaultMaxImages = 500

imagesPerPage :: Int
imagesPerPage = 42

slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start) (drop start xs)

filterOut :: (a -> Bool) -> [a] -> [a]
filterOut p = filter $ not . p

replaceSpecAmp :: String -> String
replaceSpecAmp [] = []
replaceSpecAmp ('&':'a':'m':'p':';':xs) = '&' : replaceSpecAmp xs
replaceSpecAmp (x:xs) = x : replaceSpecAmp xs

replaceSpecialChars :: String -> String
replaceSpecialChars [] = []
replaceSpecialChars (':':xs) = "%3a" ++ xs
replaceSpecialChars (x:xs) = x : replaceSpecialChars xs

lastURLComponent :: String -> String
lastURLComponent s = drop (go s 0 0) s
  where
  go [] _ final = final
  go ('/':cs) pos _ = go cs (pos+1) (pos+1)
  go (_:cs) pos final = go cs (pos+1) final