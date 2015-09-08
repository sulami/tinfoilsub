{-# LANGUAGE OverloadedStrings #-}

module Web.Server (
  runServer
) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import           Web.Scraper

import           Web.Scotty

runServer :: IO ()
runServer = scotty 3000 $ do
  get "/" $
    text "Hello, World!"
  get "/:id" $ do
    id <- param "id"
    res <- liftIO $ scrapeChannel id
    case res of
      Nothing     -> text "Hmm... invalid id?"
      Just videos -> text . TL.unlines . map title $ videos
  notFound $
    text "Four-Oh-Four"
