module Main where

import Web.Server

main :: IO ()
main = do
  channels <- lines <$> readFile "channels"
  runServer channels
