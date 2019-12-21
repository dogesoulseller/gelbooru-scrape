module Utility (slice, filterOut, replaceSpecAmp, replaceSpecialChars) where

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
