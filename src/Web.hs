{-# LANGUAGE OverloadedStrings #-}

module Web (
  runServer
  ) where

import           Web.Scotty

runServer :: IO ()
runServer = scotty 3000 $
  get "/" $
    html "Hello, World!"
