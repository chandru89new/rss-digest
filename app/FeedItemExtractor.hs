module FeedItemExtractor where

import Control.Exception (Exception (toException), SomeException)
import Control.Exception.Base (throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Data.Char (isSpace)
import Data.Text (pack, strip, unpack)
import FetchURL (fetchUrl)
import Text.HTML.TagSoup (Tag (..), fromAttrib, innerText, parseTags, partitions, (~/=), (~==))
import Text.HTML.TagSoup.Match (getTagContent)
import Text.StringLike (StringLike)

trim = unpack . strip . pack

-- Function to extract the title, href from <link>, published, and updated tags
extractData tags =
  let title = trim $ innerText $ takeBetween "<title>" "</title>" tags
      link = extractLinkHref tags
      updated = trim $ innerText $ takeBetween "<updated>" "</updated>" tags
   in FeedItem {title = title, link = link, updated = updated}

extractLinkHref tags =
  let links = extractBetweenTag "link" tags
   in case links of
        (h : _) -> Just $ fromAttrib "href" h
        _ -> Nothing

extractBetweenTag tag tags =
  let startTag = TagOpen tag []
      endTag = TagClose tag
   in takeWhile (~/= endTag) $ dropWhile (~/= startTag) tags

-- Helper function to extract inner text between specific tags
takeBetween :: String -> String -> [Tag String] -> [Tag String]
takeBetween start end tags = takeWhile (~/= end) $ dropWhile (~/= start) tags

type URL = String

-- extractFeeds :: URL -> IO (Either String [FeedItem])
extractFeeds :: URL -> ExceptT SomeException IO [FeedItem]
extractFeeds url = do
  contents <- fetchUrl url
  ExceptT . pure $ parseContents contents
  where
    parseContents c =
      let tags = parseTags c
          entryTags = partitions (~== "<entry>") tags
       in case entryTags of
            [] -> Left $ toException $ userError "No entries found."
            _ -> Right $ map extractData entryTags

data FeedItem = FeedItem {title :: String, link :: Maybe String, updated :: String} deriving (Show)

data Feed = Feed {url :: String, name :: String} deriving (Show)

{-
  for each feed link:
  get feed data -- EitherT String IO String
  parse and extract feed data -- EitherT String IO [FeedItem]
  save to DB or file.
-}