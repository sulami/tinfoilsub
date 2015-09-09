{-# LANGUAGE OverloadedStrings #-}

module Web.Server (
  runServer
) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL

import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Web.Scotty

import           Web.Scraper

runServer :: IO ()
runServer = scotty 3000 $ do
  get "/" $
    text "Hello, World!"
  get "/style.css" $
    file "static/style.css"
  get "/:id" $ do
    id <- param "id"
    res <- liftIO $ scrapeChannel id
    case res of
      Nothing     -> text "Hmm... invalid id?"
      Just videos -> html . renderHtml $ do
        H.head $
          H.link H.! A.rel "stylesheet" H.! A.href "/style.css"
        H.body $
          H.ol . forM_ videos $ H.li . H.preEscapedToHtml . showVideo
  notFound $
    text "Four-Oh-Four"
