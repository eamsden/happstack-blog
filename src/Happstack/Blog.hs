{-# LANGUAGE OverloadedStrings #-}
module Happstack.Blog (blog) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Csv
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc)
import Data.Time.Parse (strptime)
import qualified Data.Vector as V
import Happstack.Lite

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Markdown

timeFormat :: Text
timeFormat = "%Y-%m-%d %T"

data BlogEntry =
  BlogEntry
  {
    entryTitle :: Text
  , entryDate  :: UTCTime
  , entryName  :: Text
  }

-- | Serve a blog
-- Under the given path, there should be "entries.csv" with
-- title,path,date
blog :: Text -- The title of the blog
     -> Text -- the path on the server under which to find the entries.csv and markdown files
     -> ServerPart Response
blog title path = do
  entries <- parseEntries path
  msum
    [   
      pageEntries title path entries
    --, rssEntries  entries
    , blogHome title entries
    ]

instance FromRecord BlogEntry where
  parseRecord r | V.length r == 3 = 
                  do
                    title  <- r .! 0
                    mTime <- fmap fst <$> strptime timeFormat <$> r .! 1
                    ulTime <- maybe mzero return mTime
                    let uTime = localTimeToUTC utc ulTime
                    name   <- r .! 2
                    return $ BlogEntry { entryTitle = title, entryDate = uTime, entryName = name }
                | otherwise = mzero

parseEntries :: Text -> ServerPart [BlogEntry]
parseEntries path = do
  entriesBS <- liftIO $ B.readFile $ T.unpack path ++ "/entries.csv"
  let entries = decode NoHeader entriesBS 
  case entries of
    Left err -> return []
    Right entries -> return $ V.toList entries


homeLink :: H.Html
homeLink = H.a ! A.href "/" $ "[Home]"

blogHomeLink :: H.Html
blogHomeLink = H.a ! A.href "." $ "[Blog Home]"

blogHome :: Text -> [BlogEntry] -> ServerPart Response
blogHome title entries = 
  ok $ toResponse
  $ H.html $ do
      H.head $ do
        H.title $ H.toHtml title
        H.link ! A.rel "stylesheet" ! A.href "/static/web.css"
      H.body $ do
        H.div ! A.class_ "content" $ do
          H.h1 $ H.toHtml title
          homeLink
          H.ul $
            forM_ entries
              (\(BlogEntry etitle edate ename) ->
                  H.a ! A.href (H.toValue ename) ! A.id (H.toValue ename) $ H.li $ H.toHtml etitle)

pageEntries :: Text -> Text -> [BlogEntry] -> ServerPart Response
pageEntries title path entries =
    msum $ map 
      (\(BlogEntry etitle edate ename) ->
        dir (T.unpack ename) $ 
          do
            mdContents <- liftIO $ TIO.readFile $ T.unpack $ path `T.append` "/" `T.append` ename `T.append` ".md"
            ok $ toResponse $ 
              H.html $ do
                H.head $ do
                  H.title $ H.toHtml $ etitle `T.append` " - " `T.append` title
                  H.link ! A.rel "stylesheet" ! A.href "/static/web.css"
                H.body $ do
                  H.div ! A.class_ "content" $ do
                    H.h1 $ H.toHtml $ etitle `T.append` " - " `T.append` title
                    H.div ! A.class_ "timestamp" $ H.toHtml $ show edate
                    homeLink
                    " "
                    blogHomeLink
                    H.div ! A.class_ "blogentry" $ markdown def mdContents)
      entries
