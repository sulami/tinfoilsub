module Data.ConfigParser (
  parseConfig, Feed (..), Filter (..)
) where

import           Data.Char (toLower)

data Filter = Filter
  { want   :: Bool
  , string :: String
  } deriving (Show)

data Feed = Feed
  { name    :: String
  , filters :: [Filter]
  } deriving (Show)

parseConfig :: String -> [Feed]
parseConfig = map parseEntry . filter (not . null) . lines

parseEntry :: String -> Feed
parseEntry line = let whole = words $ map toLower line
                      name = head whole
                      filters = drop 1 whole
                  in Feed name $ parseFilters filters

parseFilters :: [String] -> [Filter]
parseFilters []       = []
parseFilters ("+":xs) = let (fws,rest) = span (\x -> x /= "+" && x /= "-") xs
                        in Filter True (unwords fws) : parseFilters rest
parseFilters ("-":xs) = let (fws,rest) = span (\x -> x /= "+" && x /= "-") xs
                        in Filter False (unwords fws) : parseFilters rest
parseFilters (x:xs)   = parseFilters xs

