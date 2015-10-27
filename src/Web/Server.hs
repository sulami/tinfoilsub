{-# LANGUAGE OverloadedStrings #-}

module Web.Server (
  runServer
) where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (sort)
import           Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL

import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Web.Scotty

import           Data.ConfigParser
import           Web.Scraper

runServer :: [Feed] -> IO ()
runServer feeds = scotty 3000 $ do
  get "/" $ do
    videos <- fmap (sort . concat) . liftIO $
                mapConcurrently scrapeChannel feeds
    html . renderVideos $ take 50 videos
  get "/video/:id" $ do
    vid <- param "id" :: ActionM String
    html $ renderVideo vid
  get "/style.css" $
    file "static/style.css"
  notFound $
    text "Four-Oh-Four"

renderVideos :: [Video] -> TL.Text
renderVideos videos = renderHtml $ do
  H.head $ do
    H.title "TinfoilSub"
    H.link H.! A.rel "stylesheet" H.! A.href "/style.css"
  H.body $ do
    H.h1 "TinfoilSub"
    H.ol . forM_ videos $ H.li . H.preEscapedToHtml . showVideo

renderVideo :: String -> TL.Text
renderVideo vid = renderHtml $ do
  H.head $ do
    H.title "TinfoilSub"
    H.link H.! A.rel "stylesheet" H.! A.href "/style.css"
  let url = "<iframe src='https://youtube.com/embed/" ++ vid ++ "'></iframe>"
  H.body $ H.preEscapedToHtml url

