module Utility (slice, filterOut, replaceSpecAmp, replaceSpecialChars, lastURLComponent) where

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
  go (c:cs) pos final
    | c == '/' = go cs (pos+1) (pos+1)
    | otherwise = go cs (pos+1) final