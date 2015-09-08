{-# LANGUAGE OverloadedStrings #-}

module Web.Server (
  runServer
) where

import           Web.Scotty

runServer :: IO ()
runServer = scotty 3000 $
  get "/" $
    html "Hello, World!"
