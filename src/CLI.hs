module CLI (printUsage, getTags) where

printUsage :: IO ()
printUsage = mapM_ putStr ["\nUsage:\n"
  , "gelbooru-scrape [options] tags\n"
  , "Tags are a comma-separated list of tags to include in the search\n"
  , "To exclude a tag, prefix it with '-', eg. blue_hair,-long_hair\n"
  , "Options:\n"
  , "    None yet\n"]

getTags :: [String] -> String
getTags = last
