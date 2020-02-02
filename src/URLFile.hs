module URLFile (loadURLFile) where

import Utility (filterOut)
import Data.Char (isSpace)
import Control.Monad (when)

trimEmptyLines :: [String] -> [String]
trimEmptyLines = filterOut (all isSpace)

loadURLFile :: String -> IO [String]
loadURLFile filePath = do
  fileContents <- readFile filePath
  when (null fileContents) (errorWithoutStackTrace "URL file is empty")
  return $ (trimEmptyLines . lines) fileContents