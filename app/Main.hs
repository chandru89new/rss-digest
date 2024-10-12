{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (Exception (displayException, toException), SomeException (SomeException), throw, throwIO, try)
import Control.Exception.Base (throw)
import Control.Monad (liftM, unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (..), runExcept, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.ByteString.Char8 as ByteString (ByteString, unpack)
import Data.Char (isSpace)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool, createPool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.Pool.Introspection (PoolConfig)
import Data.String (IsString (fromString))
import Data.Text (pack, strip, unpack)
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, parseTimeM)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Query (Query), ToRow, close, execute, execute_, field, open, query_, toRow, withConnection)
import GHC.Base (join)
import Network.HTTP.Simple (Request, Response, getResponseBody, httpBS, parseRequest)
import Text.HTML.TagSoup (Tag (..), fromAttrib, innerText, parseTags, partitions, (~/=), (~==))
import Text.HTML.TagSoup.Match (getTagContent)
import Text.StringLike (StringLike)

main = runApp $ initDB >> updateAllFeeds

fetchUrl :: String -> ExceptT AppError IO String
fetchUrl url = ExceptT $ do
  req <- (try' FetchError $ parseRequest url) :: IO (Either AppError Request)
  case req of
    Left e -> pure $ Left e
    Right _req -> do
      res <- try' FetchError $ httpBS _req :: IO (Either AppError (Response ByteString))
      pure $ fmap (ByteString.unpack . getResponseBody) res

type App a = Config -> ExceptT AppError IO a

data AppError
  = FetchError String
  | DatabaseError String
  | FeedParseError String
  | GeneralError String
  deriving (Show)

newtype Config = Config
  {connPool :: Pool Connection}

runApp :: App a -> IO ()
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
      link = nothingIfEmpty . getInnerText $ takeBetween "<link>" "</link>" tags
      pubDate = nothingIfEmpty . getInnerText $ takeBetween "<pubDate>" "</pubDate>" tags
      updatedDate = nothingIfEmpty . getInnerText $ takeBetween "<updated>" "</updated>" tags
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
extractFeedItems :: URL -> String -> ExceptT AppError IO [FeedItem]
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

data FeedItem = FeedItem {title :: String, link :: Maybe String, updated :: Maybe Day} deriving (Eq, Show)

data Feed = Feed {url :: String, name :: String} deriving (Show)

parseDate :: String -> Maybe Day
parseDate datetime = fmap utctDay $ firstJust $ map tryParse [fmt1, fmt2]
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

justRunQuery :: Connection -> Query -> ExceptT AppError IO ()
justRunQuery conn query =
  ExceptT $ tryExecute_ conn query

initializeTables :: Connection -> ExceptT AppError IO ()
initializeTables conn = do
  justRunQuery conn createFeedsTable
  justRunQuery conn createFeedItemsTable

initDB :: App ()
initDB config = do
  let Config connPool = config
  ExceptT $ withResource connPool initTable
  where
    initTable :: Connection -> IO (Either AppError ())
    initTable conn = runExceptT $ initializeTables conn

insertFeedItem :: Connection -> FeedItem -> IO (Either AppError FeedItem)
insertFeedItem conn feedItem@FeedItem {..} = do
  let unwrappedLink = fromMaybe "" link
  rows <- try' DatabaseError $ query_ conn (queryToCheckIfItemExists unwrappedLink) :: IO (Either AppError [FeedItem])
  case rows of
    Right (x : _) -> pure $ Left $ DatabaseError ("Post already exists in the database: " ++ unwrappedLink ++ ".")
    _ -> do
      res <- tryExecute conn insertFeedQuery $ toRow (title, link, updated) :: IO (Either AppError ())
      pure $ fmap (const feedItem) res

instance FromRow FeedItem where
  fromRow = FeedItem <$> field <*> field <*> field

tryExecute_ :: Connection -> Query -> IO (Either AppError ())
tryExecute_ conn = try' DatabaseError . execute_ conn

tryExecute :: forall q. ToRow q => Connection -> Query -> q -> IO (Either AppError ())
tryExecute conn query q = try' DatabaseError $ execute conn query q

tryQuery_ :: FromRow a => Connection -> Query -> IO (Either AppError [a])
tryQuery_ conn = try' DatabaseError . query_ conn

insertFeed :: URL -> App ()
insertFeed feedUrl config = do
  let Config connPool = config
      query = fromString $ "INSERT INTO feeds (url) VALUES ('" ++ feedUrl ++ "');"
  ExceptT $ withResource connPool $ \conn -> do
    result <- tryExecute_ conn query :: IO (Either AppError ())
    pure $ void result

getFeedUrlsFromDB :: App [URL]
getFeedUrlsFromDB config = do
  let Config connPool = config
  res <- liftIO ((try' DatabaseError $ withResource connPool handleQuery) :: IO (Either AppError [URL]))
  ExceptT $ pure res
  where
    handleQuery :: Connection -> IO [URL]
    handleQuery conn = do
      rows <- query_ conn selectUrlFromFeeds :: IO [(Int, String)]
      pure $ map snd rows

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
    \ url TEXT NOT NULL, \
    \ state TEXT DEFAULT 'enabled' \
    \);"

selectUrlFromFeeds :: Query
selectUrlFromFeeds = fromString "SELECT id, url FROM feeds;"

queryToCheckIfItemExists :: String -> Query
queryToCheckIfItemExists link = fromString $ "select link, title, updated from feed_items where link = '" ++ link ++ "' limit 1;"

insertFeedQuery = fromString "INSERT INTO feed_items (title, link, updated) VALUES (?, ?, ?);" :: Query

processFeed :: URL -> App ()
processFeed url (Config connPool) = do
  _ <- liftIO $ putStrLn $ "Processing feed: " ++ url
  contents <- fetchUrl url
  feedItems <- extractFeedItems url contents
  res <- liftIO ((try' DatabaseError $ withResource connPool $ \conn -> mapM (handleInsert conn) feedItems) :: IO (Either AppError [Int]))
  when (isRight res) $ liftIO $ putStrLn $ "Finished processing " ++ url ++ ". Discovered " ++ show (length feedItems) ++ " items. Added " ++ show (sum $ fromRight [] res) ++ " items (duplicates are ignored)."
  ExceptT $ pure (either (Left . DatabaseError . show) (Right . const ()) res)
  where
    handleInsert :: Connection -> FeedItem -> IO Int
    handleInsert conn feedItem = do
      res <- insertFeedItem conn feedItem
      -- when (isLeft res) $ print (fromLeft (DatabaseError "Error inserting feed item") res)
      pure $ if isLeft res then 0 else 1

processFeeds :: [URL] -> App ()
processFeeds urls config = mapM_ (flip processFeed config) urls

updateAllFeeds :: App ()
updateAllFeeds config = do
  urls <- getFeedUrlsFromDB config
  processFeeds urls config

nothingIfEmpty :: Foldable t => t a -> Maybe (t a)
nothingIfEmpty a = if null a then Nothing else Just a

try' :: (String -> AppError) -> IO a -> IO (Either AppError a)
try' mkError action = do
  res <- (try :: IO a -> IO (Either SomeException a)) action
  pure $ case res of
    Left e -> Left $ mkError $ show e
    Right a -> Right a
