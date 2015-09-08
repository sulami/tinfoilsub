module Web.Scraper (
  scrapeChannel
) where

import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as B

import           Control.Lens ((^.))
import           Network.Wreq (get, responseBody)
import           Text.HTML.Scalpel

data Video = Video {
  -- uploader :: Text,
  title    :: Text
  -- id       :: Text,
  -- length   :: Int,
  -- time     :: Int
} deriving (Show, Eq)

scrapeChannel :: String -> IO (Maybe [Video])
scrapeChannel id = do
  page <- videoPage id
  return $ scrapePage page

videoPage :: String -> IO B.ByteString
videoPage id = do
  res <- get $ channelUrl id
  return $ res ^. responseBody
  where
    channelUrl :: String -> String
    channelUrl id = "https://youtube.com/user/" ++ id ++ "/videos"

scrapePage :: B.ByteString -> Maybe [Video]
scrapePage page = scrapeStringLike page videos
  where
    videos :: Scraper B.ByteString [Video]
    videos = chroots ("li" @: [hasClass "channels-content-item"]) video

    video :: Scraper B.ByteString Video
    video = do
      title <- fmap decodeUtf8 $ text $ "a" @: [hasClass "yt-uix-title-link"]
      return $ Video title

