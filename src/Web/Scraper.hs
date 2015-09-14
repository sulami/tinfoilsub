module Web.Scraper (
  scrapeChannel, showVideo, Video
) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import           Data.Ord (comparing)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (decodeUtf8)

import           Network.Curl.Opts (CurlOption (CurlHttpHeaders))
import           Text.HTML.Scalpel

data Video = Video {
  uploader :: !TL.Text,
  title    :: !TL.Text,
  url      :: !TL.Text,
  len      :: !TL.Text,
  thumb    :: !TL.Text,
  time     :: Int
} deriving (Show, Eq)

instance Ord Video where
  compare = comparing time

showVideo :: Video -> TL.Text
showVideo vid = TL.unwords [
  TL.concat [TL.pack "<a target='_blank' href='/video/",
             last . TL.split (== '=') $ url vid, TL.pack "'>"],
  TL.concat [TL.pack "<img src='", thumb vid, TL.pack "' /><br />"],
  TL.concat [ TL.pack "[", len vid, TL.pack "]" ],
  uploader vid,
  TL.pack "-",
  title vid,
  TL.pack "</a>" ]

scrapeChannel :: String -> IO (Maybe [Video])
scrapeChannel id = scrapeURLWithOpts requestHeader (channelURL id) videos
  where
    requestHeader :: [CurlOption]
    requestHeader = [CurlHttpHeaders [ "Host: www.youtube.com",
                                       "Accept-Language: en-us,en;q=0.5" ]]

    channelURL :: String -> String
    channelURL id = "https://youtube.com/user/" ++ id ++ "/videos"

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
      thumb <- decodeUtf8 <$> attr "src" "img"
      time  <- last <$> texts ("ul" @: [hasClass "yt-lockup-meta-info"] // "li")
      return $ Video uploader title url len thumb $ parseTime time

    parseTime :: BL.ByteString -> Int
    parseTime t = case words (BLC8.unpack t) of
                    h : ["hour",  "ago"] -> read h
                    h : ["hours", "ago"] -> read h
                    d : ["day",   "ago"] -> read d * 24
                    d : ["days",  "ago"] -> read d * 24
                    w : ["week",  "ago"] -> read w * 24 * 7
                    w : ["weeks", "ago"] -> read w * 24 * 7
                    m : ["month", "ago"] -> read m * 24 * 30
                    m : ["months","ago"] -> read m * 24 * 30
                    y : ["year",  "ago"] -> read y * 24 * 30 * 12
                    y : ["years", "ago"] -> read y * 24 * 30 * 12
                    _                    -> 0

