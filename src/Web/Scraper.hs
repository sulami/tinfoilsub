module Web.Scraper (
  scrapeChannel, displayVideo
) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding (decodeUtf8)

import           Control.Lens ((^.))
import           Network.Wreq (get, responseBody)
import           Text.HTML.Scalpel

data Video = Video {
  uploader :: Text,
  title    :: Text,
  url      :: Text
  -- length   :: Int,
  -- time     :: Int
} deriving (Show, Eq)

displayVideo :: Video -> Text
displayVideo vid = Data.Text.Lazy.unwords [uploader vid,
  pack "-",
  title vid,
  pack ":",
  url vid]

scrapeChannel :: String -> IO (Maybe [Video])
scrapeChannel id = do
  page <- videoPage id
  return $ scrapePage page

videoPage :: String -> IO BL.ByteString
videoPage id = do
  res <- get $ channelUrl id
  return $ res ^. responseBody
  where
    channelUrl :: String -> String
    channelUrl id = "https://youtube.com/user/" ++ id ++ "/videos"

scrapePage :: BL.ByteString -> Maybe [Video]
scrapePage page = scrapeStringLike page videos
  where
    videos :: Scraper BL.ByteString [Video]
    videos = do
      uploader <- fmap decodeUtf8 getUploader
      chroots ("li" @: [hasClass "channels-content-item"]) $ video uploader

    getUploader :: Scraper BL.ByteString BL.ByteString
    getUploader = text $ "a" @: [hasClass "branded-page-header-title-link"]

    video :: Text -> Scraper BL.ByteString Video
    video uploader = do
      let textBox = "a" @: [hasClass "yt-ui-ellipsis"]
      title <- decodeUtf8 <$> text textBox
      url   <- decodeUtf8 <$> attr "href" textBox
      return $ Video uploader title url

