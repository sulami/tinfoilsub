module Web.Scraper (
  scrapeChannel, displayVideo
) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (decodeUtf8)

import           Control.Lens ((^.))
import           Network.Wreq (get, responseBody)
import           Text.HTML.Scalpel

data Video = Video {
  uploader :: TL.Text,
  title    :: TL.Text,
  url      :: TL.Text,
  len      :: TL.Text,
  time     :: TL.Text
} deriving (Show, Eq)

displayVideo :: Video -> TL.Text
displayVideo vid = TL.unwords [
  TL.pack "<li>",
  TL.concat [time vid, TL.pack ":"],
  TL.concat [TL.pack "<a href='https://youtube.com", url vid, TL.pack "'>"],
  uploader vid,
  TL.pack "-",
  title vid,
  TL.concat [ TL.pack "[", len vid, TL.pack "]" ],
  TL.pack "</a></li>" ]

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

    video :: TL.Text -> Scraper BL.ByteString Video
    video uploader = do
      let textBox = "a" @: [hasClass "yt-ui-ellipsis"]
      title <- decodeUtf8 <$> text textBox
      url   <- decodeUtf8 <$> attr "href" textBox
      len   <- decodeUtf8 <$> text ("span" @: [hasClass "video-time"] // "span")
      time  <- decodeUtf8 . last <$>
                 texts ("ul" @: [hasClass "yt-lockup-meta-info"] // "li")
      return $ Video uploader title url len time

