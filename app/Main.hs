module Main where

import           Data.ConfigParser
import           Web.Server

main :: IO ()
main = do
  feeds <- parseConfig <$> readFile "channels"
  runServer feeds

