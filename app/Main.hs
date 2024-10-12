{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (Exception (toException), SomeException (SomeException), throw, throwIO, try)
import Control.Exception.Base (throw)
import Control.Monad (liftM, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (..), runExcept, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.ByteString.Char8 as ByteString (ByteString, unpack)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool, createPool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.Pool.Introspection (PoolConfig)
import Data.String (IsString (fromString))
import Data.Text (pack, strip, unpack)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Query (Query), close, execute, execute_, field, open, query_, toRow, withConnection)
import GHC.Base (join)
import Network.HTTP.Simple (Request, Response, getResponseBody, httpBS, parseRequest)
import Text.HTML.TagSoup (Tag (..), fromAttrib, innerText, parseTags, partitions, (~/=), (~==))
import Text.HTML.TagSoup.Match (getTagContent)
import Text.StringLike (StringLike)

fetchUrl :: String -> ExceptT (AppError String) IO String
fetchUrl url = ExceptT $ do
  req <- parseRequest url
  res <- try $ httpBS req :: IO (Either SomeException (Response ByteString))
  pure $ either (Left . FetchError . show) (Right . ByteString.unpack . getResponseBody) res

type App e a = Config -> ExceptT (AppError e) IO a

data AppError e
  = FetchError String
  | DatabaseError String e
  | FeedParseError String
  | GeneralError String
  deriving (Show)

newtype Config = Config
  {connPool :: Pool Connection}

runApp :: Show e => App e a -> IO ()
runApp app = do
  pool <- newPool (defaultPoolConfig (open "./feeds.db") close 60.0 10)
  let config = Config {connPool = pool}
  res <- runExceptT (app config)
  case res of
    Left err -> print err
    Right _ -> return ()
  destroyAllResources pool

trim = Data.Text.unpack . strip . pack

getInnerText = trim . innerText

-- Function to extract the title, href from <link>, published, and updated tags
extractFeedItem :: [Tag String] -> FeedItem
extractFeedItem tags =
  let title = getInnerText $ takeBetween "<title>" "</title>" tags
      linkFromYtFeed = extractLinkHref tags -- youtube specific
      link = case getInnerText $ takeBetween "<link>" "</link>" tags of
        "" -> Nothing
        x -> Just x
      pubDate = case getInnerText $ takeBetween "<pubDate>" "</pubDate>" tags of
        "" -> Nothing
        x -> Just x
      updatedDate = case getInnerText $ takeBetween "<updated>" "</updated>" tags of
        "" -> Nothing
        x -> Just x
      updated = pubDate <|> updatedDate
   in FeedItem {title = title, link = link <|> linkFromYtFeed, updated = updated >>= parseDate}

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

-- extractFeedItems :: URL -> IO (Either String [FeedItem])
extractFeedItems :: URL -> String -> ExceptT (AppError e) IO [FeedItem]
extractFeedItems url contents = ExceptT $ do
  pure $ parseContents contents
  where
    parseContents c =
      let tags = parseTags c
          entryTags = partitions (~== "<entry>") tags -- this is for youtube feeds only
          itemTags = partitions (~== "<item>") tags
       in case (itemTags, entryTags) of
            ([], []) -> Left $ FeedParseError $ "No items found on link: " ++ url ++ "."
            (xs, []) -> Right $ map extractFeedItem xs
            ([], ys) -> Right $ map extractFeedItem ys
            (xs, ys) -> Right $ map extractFeedItem (xs ++ ys)

data FeedItem = FeedItem {title :: String, link :: Maybe String, updated :: Maybe UTCTime} deriving (Eq, Show)

data Feed = Feed {url :: String, name :: String} deriving (Show)

{-
  for each feed link:
  get feed data -- EitherT String IO String
  parse and extract feed data -- EitherT String IO [FeedItem]
  save to DB or file.
-}

parseDate :: String -> Maybe UTCTime
parseDate datetime = firstJust $ map tryParse [fmt1, fmt2]
  where
    fmt1 = "%Y-%m-%dT%H:%M:%S%z"
    fmt2 = "%a, %d %b %Y %H:%M:%S %z"
    tryParse fmt = parseTimeM True defaultTimeLocale fmt datetime :: Maybe UTCTime
    firstJust :: [Maybe a] -> Maybe a
    firstJust xs = go xs Nothing
      where
        go [] acc = acc
        go (x : xs) acc = case x of
          Just y -> Just y
          Nothing -> go xs acc

dbFile = "./feeds.db"

-- SQL query to create the `feed_items` table if it doesn't already exist

justRunQuery :: Connection -> Query -> ExceptT SomeException IO ()
justRunQuery conn query =
  ExceptT $ try $ execute_ conn query

initializeTables :: Connection -> ExceptT SomeException IO ()
initializeTables conn = do
  justRunQuery conn createFeedsTable
  justRunQuery conn createFeedItemsTable

-- initDB :: ExceptT SomeException IO ()
-- initDB = do
--   conn <- openConn
--   initializeTables conn
--   liftIO $ close conn

initDB :: App String ()
initDB config = do
  let Config connPool = config
  ExceptT $ withResource connPool initTable
  where
    initTable :: Connection -> IO (Either (AppError String) ())
    initTable conn = do
      result <- runExceptT $ initializeTables conn
      pure $ either (Left . toDBError "Error: ") Right result

insertFeedItem :: Connection -> FeedItem -> ExceptT (AppError String) IO FeedItem
insertFeedItem conn feedItem@FeedItem {..} = ExceptT $ do
  let unwrappedLink = fromMaybe "" link
  rows <- try $ query_ conn (queryToCheckIfItemExists unwrappedLink) :: IO (Either SomeException [FeedItem])
  case rows of
    Left e -> pure $ Left $ DatabaseError "Error when trying to find if feed is already in table ->" (show e)
    Right (x : _) -> pure $ Left $ DatabaseError "Post already exists ->" unwrappedLink
    Right [] -> do
      res <- try $ execute conn insertFeedQuery $ toRow (title, link, updated) :: IO (Either SomeException ())
      pure $ case res of
        Left e -> Left $ DatabaseError ("Could not add" ++ unwrappedLink) (show e)
        Right _ -> Right feedItem

-- safeDBQuery :: FromRow a => Connection -> Query -> ExceptT (AppError String) IO [a]
-- safeDBQuery conn query = ExceptT $ do
--   result <- try $ query_ conn query :: IO (Either SomeException [a])
--   pure $ case result of
--     Left e -> Left $ DatabaseError "Error when querying the database ->" (show e)
--     Right x -> Right x

instance FromRow FeedItem where
  fromRow = FeedItem <$> field <*> field <*> field

toDBError :: Show e => e -> SomeException -> AppError e
toDBError item = flip DatabaseError item . show

insertFeed :: URL -> App URL ()
insertFeed feedUrl config = do
  let Config connPool = config
      query = fromString $ "INSERT INTO feeds (url) VALUES ('" ++ feedUrl ++ "');"
  ExceptT $ withResource connPool $ \conn -> do
    result <- try $ execute_ conn query :: IO (Either SomeException ())
    pure $ case result of
      Left e -> Left $ toDBError feedUrl e
      Right _ -> Right ()

-- queries

createFeedItemsTable :: Query
createFeedItemsTable =
  fromString
    "CREATE TABLE IF NOT EXISTS feed_items (\
    \ link TEXT NOT NULL PRIMARY KEY, \
    \ title TEXT NOT NULL, \
    \ updated DATETIME DEFAULT CURRENT_TIMESTAMP, \
    \ state TEXT DEFAULT 'unread'  \
    \);"

createFeedsTable :: Query
createFeedsTable =
  fromString
    "CREATE TABLE IF NOT EXISTS feeds (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \
    \ url TEXT NOT NULL \
    \);"

selectUrlFromFeeds :: Query
selectUrlFromFeeds = fromString "SELECT url FROM feeds;"

queryToCheckIfItemExists :: String -> Query
queryToCheckIfItemExists link = fromString $ "selectt link, title, updated from feed_items where link = '" ++ link ++ "' limit 1;"

insertFeedQuery = fromString "INSERT INTO feed_items (title, link, updated) VALUES (?, ?, ?);" :: Query

main = putStrLn "Hello, Haskell!"

{-
processFeed :
- fetch contents from feed url :: String
- parse contents :: [FeedItem]
- for each feeditem, insert into db
-}

-- processFeed :: URL -> Connection -> App String ()
processFeed :: URL -> App String ()
processFeed url config = do
  contents <- fetchUrl url
  feedItems <- extractFeedItems url contents
  let Config connPool = config
  liftIO $ withResource connPool $ \conn -> mapM_ (handleInsert conn) feedItems
  where
    handleInsert :: Connection -> FeedItem -> IO ()
    handleInsert conn feedItem = do
      res <- runExceptT $ insertFeedItem conn feedItem
      case res of
        Left e -> print e
        Right _ -> return ()

processFeeds :: [URL] -> App String ()
processFeeds urls config = mapM_ (flip processFeed config) urls
