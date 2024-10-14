{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception (Exception, SomeException, throw, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import Data.ByteString.Char8 as ByteString (unpack)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.FileEmbed (embedFile)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.String (IsString (fromString))
import Data.Text (pack, replace, strip, unpack)
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Query, close, execute, execute_, field, open, query_, toRow)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Network.URI (URI (uriAuthority, uriScheme), URIAuth (..), parseURI)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.HTML.TagSoup (Tag (..), fromAttrib, innerText, parseTags, partitions, (~/=), (~==))

main :: IO ()
main = do
  command <- getCommand
  unless (command == InvalidCommand) $
    runApp
      ( \config -> do
          _ <- initDB config
          pure ()
      )
  case command of
    AddFeed link -> do
      let url = parseURL link
      case url of
        Just _url -> do
          runApp $ \config -> do
            res <- (try :: IO a -> IO (Either SomeException a)) $ insertFeed _url config
            either print (const $ putStrLn "Added.") res
        Nothing -> putStrLn "Invalid URL."
    RemoveFeed url -> runApp $ \config -> do
      input <- userConfirmation "This will remove the feed and all the posts associated with it."
      if input
        then do
          res <- (try :: IO a -> IO (Either SomeException a)) $ removeFeed url config
          either print (const $ putStrLn "Done.") res
        else putStrLn "Cancelled."
    ListFeeds -> runApp $ \config -> do
      feeds <- listFeeds config >>= pure . concat
      putStrLn $ intercalate "\n" feeds
    RefreshFeeds -> do
      runApp $ \config -> do
        updateAllFeeds config
        putStrLn "Done."
    CreateDigest dateString -> do
      today <- fmap utctDay getCurrentTime
      digestDay <- case dateString of
        Just date -> (try :: IO a -> IO (Either SomeException a)) (parseTimeM True defaultTimeLocale "%Y-%m-%d" date :: IO Day)
        Nothing -> pure $ Right today
      case digestDay of
        Right day -> do
          runApp $ \config@Config {..} -> do
            items <- createDigest day config
            if null items
              then putStrLn $ "No digest for " ++ show day ++ "."
              else do
                putStrLn $ "Preparing digest for " ++ show day ++ ":"
                file <- writeDigest template day items
                putStrLn $ "Digest written to " ++ file ++ "."
        Left _ -> do
          putStrLn "Invalid date format. Use YYYY-MM-DD or leave empty to get today's digest. (e.g 2020-01-01)"
    PurgeEverything -> do
      input <- userConfirmation "This will remove all feeds and all the posts associated with them."
      if input
        then do
          putStrLn "Nuking everything..."
          _ <- runApp destroyDB
          putStrLn "Fin."
        else putStrLn "Cancelled."
    InvalidCommand -> putStrLn progHelp

progHelp :: String
progHelp =
  "Usage: rss-digest <command> [args]\n\
  \Commands:\n\
  \  help - Show this help.\n\
  \  add <url> - Add a feed. <url> must be valid HTTP(S) URL.\n\
  \  remove <url> - Remove a feed and all its associated posts with the given url.\n\
  \  digest - Generate the digest for today.\n\
  \  digest <date> - Generate the digest for a given [date] in the YYYY-MM-DD format.\n\
  \  list feeds - List all feeds\n\
  \  refresh - Refresh all feeds\n\
  \  purge - Purge everything\n"

data Command
  = AddFeed URL
  | RefreshFeeds
  | ListFeeds
  | RemoveFeed URL
  | CreateDigest (Maybe String)
  | PurgeEverything
  | InvalidCommand
  deriving (Eq)

getCommand :: IO Command
getCommand = do
  args <- getArgs
  pure $ case args of
    ("add" : url : _) -> AddFeed url
    ("remove" : url : _) -> RemoveFeed url
    ("list" : "feeds" : _) -> ListFeeds
    ("refresh" : _) -> RefreshFeeds
    ("digest" : xs) -> CreateDigest (if null xs then Nothing else Just $ head xs)
    ("purge" : _) -> PurgeEverything
    ("help" : _) -> InvalidCommand
    _ -> InvalidCommand

parseURL :: String -> Maybe URL
parseURL url = case parseURI url of
  Just uri -> (if uriScheme uri `elem` ["http:", "https:"] then Just url else Nothing)
  Nothing -> Nothing

failWith :: (String -> AppError) -> IO a -> IO a
failWith mkError action = do
  res <- (try :: IO a -> IO (Either SomeException a)) action
  pure $ either (throw . mkError . show) id res

fetchUrl :: String -> IO String
fetchUrl url = do
  res <- failWith FetchError $ do
    req <- parseRequest url
    httpBS req
  pure $ (ByteString.unpack . getResponseBody) res

type App a = Config -> IO a

data AppError
  = FetchError String
  | DatabaseError String
  | FeedParseError String
  | DigestError String
  | GeneralError String
  deriving (Show)

instance Exception AppError

data Config = Config
  {connPool :: Pool Connection, template :: String}

runApp :: App a -> IO ()
runApp app = do
  let template = $(embedFile "./template.html")
  pool <- newPool (defaultPoolConfig (open dbFile) close 60.0 10)
  let config = Config {connPool = pool, template = ByteString.unpack template}
  res <- (try :: IO a -> IO (Either AppError a)) $ app config
  destroyAllResources pool
  either throw (const $ return ()) res

trim :: String -> String
trim = Data.Text.unpack . strip . pack

getInnerText :: [Tag String] -> String
getInnerText = trim . innerText

extractFeedItem :: [Tag String] -> FeedItem
extractFeedItem tags =
  let title = getInnerText $ takeBetween "<title>" "</title>" tags
      linkFromYtFeed = extractLinkHref tags -- youtube specific
      link = nothingIfEmpty . getInnerText $ takeBetween "<link>" "</link>" tags
      pubDate = nothingIfEmpty . getInnerText $ takeBetween "<pubDate>" "</pubDate>" tags
      updatedDate = nothingIfEmpty . getInnerText $ takeBetween "<updated>" "</updated>" tags
      updated = pubDate <|> updatedDate
   in FeedItem {title = title, link = link <|> linkFromYtFeed, updated = updated >>= parseDate}

extractLinkHref :: [Tag String] -> Maybe String
extractLinkHref tags =
  let links = extractBetweenTag "link" tags
   in case links of
        (h : _) -> Just $ fromAttrib "href" h
        _ -> Nothing

extractBetweenTag :: String -> [Tag String] -> [Tag String]
extractBetweenTag tag tags =
  let startTag = TagOpen tag []
      endTag = TagClose tag
   in takeWhile (~/= endTag) $ dropWhile (~/= startTag) tags

takeBetween :: String -> String -> [Tag String] -> [Tag String]
takeBetween start end tags = takeWhile (~/= end) $ dropWhile (~/= start) tags

type URL = String

extractFeedItems :: String -> Maybe [FeedItem]
extractFeedItems = parseContents
  where
    parseContents c =
      let tags = parseTags c
          entryTags = partitions (~== "<entry>") tags -- this is for youtube feeds only
          itemTags = partitions (~== "<item>") tags
       in case (itemTags, entryTags) of
            ([], []) -> Nothing
            (xs, []) -> Just $ map extractFeedItem xs
            ([], ys) -> Just $ map extractFeedItem ys
            (xs, ys) -> Just $ map extractFeedItem (xs ++ ys)

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
        go (x : xs_) acc = case x of
          Just y -> Just y
          Nothing -> go xs_ acc

dbFile :: String
dbFile = "./feeds.db"

justRunQuery :: Connection -> Query -> IO ()
justRunQuery conn query = do
  _ <- failWith DatabaseError $ execute_ conn query
  pure ()

initializeTables :: Connection -> IO ()
initializeTables conn = do
  _ <- justRunQuery conn createFeedsTable
  justRunQuery conn createFeedItemsTable

initDB :: App ()
initDB (Config {..}) = do
  withResource
    connPool
    ( \conn -> do
        _ <- setPragmas conn
        initializeTables conn
        runMigrations conn
    )

insertFeedItem :: Connection -> (Int, Day, FeedItem) -> IO (Maybe FeedItem)
insertFeedItem conn (feedId, addedOn, feedItem@FeedItem {..}) = do
  let unwrappedLink = fromMaybe "" link
  rows <- failWith DatabaseError $ query_ conn (queryToCheckIfItemExists unwrappedLink) :: IO [FeedItem]
  case rows of
    (_ : _) -> pure Nothing
    _ -> do
      _ <- failWith DatabaseError $ execute conn insertFeedQuery $ toRow (title, link, updated, feedId, addedOn) :: IO ()
      pure (Just feedItem)

instance FromRow FeedItem where
  fromRow = FeedItem <$> field <*> field <*> field

insertFeed :: URL -> App ()
insertFeed feedUrl (Config {..}) = do
  let query = fromString $ "INSERT INTO feeds (url) VALUES ('" ++ feedUrl ++ "');"
  withResource connPool $ \conn -> failWith DatabaseError $ execute_ conn query

getFeedUrlsFromDB :: App [(Int, URL)]
getFeedUrlsFromDB (Config {..}) = failWith DatabaseError $ withResource connPool handleQuery
  where
    handleQuery :: Connection -> IO [(Int, URL)]
    handleQuery conn = do
      query_ conn selectUrlFromFeeds :: IO [(Int, String)]

createFeedItemsTable :: Query
createFeedItemsTable =
  fromString
    "CREATE TABLE IF NOT EXISTS feed_items (\
    \ link TEXT NOT NULL PRIMARY KEY, \
    \ title TEXT NOT NULL, \
    \ updated DATETIME DEFAULT CURRENT_TIMESTAMP, \
    \ state TEXT DEFAULT 'unread',  \
    \ feed_id INTEGER NOT NULL,  \
    \ FOREIGN KEY (feed_id) REFERENCES feeds(id) ON DELETE CASCADE \
    \);"

createFeedsTable :: Query
createFeedsTable =
  fromString
    "CREATE TABLE IF NOT EXISTS feeds (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \
    \ url TEXT NOT NULL UNIQUE, \
    \ state TEXT DEFAULT 'enabled' \
    \);"

selectUrlFromFeeds :: Query
selectUrlFromFeeds = fromString "SELECT id, url FROM feeds where state = 'enabled';"

selectAllFeeds :: Query
selectAllFeeds = fromString "SELECT url FROM feeds;"

queryToCheckIfItemExists :: String -> Query
queryToCheckIfItemExists link = fromString $ "select link, title, updated from feed_items where link = '" ++ link ++ "' limit 1;"

insertFeedQuery :: Query
insertFeedQuery = fromString "INSERT INTO feed_items (title, link, updated, feed_id, created_at) VALUES (?, ?, ?, ?, ?);" :: Query

processFeed :: (Int, URL) -> Day -> App ()
processFeed (feedId, url) addedOn (Config {..}) = do
  _ <- liftIO $ putStrLn $ "Processing feed: " ++ url
  contents <- fetchUrl url
  let feedItems = extractFeedItems contents
      unwrappedFeedItems = fromMaybe [] feedItems
  when (isNothing feedItems) $ putStrLn $ "No posts found on link: " ++ url ++ "."
  res <-
    ( try $ failWith DatabaseError $ withResource connPool $ \conn -> do
        _ <- setPragmas conn
        mapM (handleInsert conn) unwrappedFeedItems
    ) ::
      IO (Either AppError [Int])
  when ((not . null) unwrappedFeedItems && isRight res) $ liftIO $ putStrLn $ "Finished processing " ++ url ++ ". Discovered " ++ show (length unwrappedFeedItems) ++ " posts. Added " ++ show (sum $ fromRight [] res) ++ " posts (duplicates are ignored)."
  where
    handleInsert :: Connection -> FeedItem -> IO Int
    handleInsert conn feedItem = do
      res <- (try $ insertFeedItem conn (feedId, addedOn, feedItem)) :: IO (Either AppError (Maybe FeedItem))
      when (isLeft res) $ print (fromLeft (DatabaseError "Error inserting feed item") res)
      pure $ either (const 0) (\r -> if isJust r then 1 else 0) res

processFeeds :: [(Int, URL)] -> App ()
processFeeds urls config = do
  utcTime <- getCurrentTime
  let addedOn = utctDay utcTime
  mapM_ (\url -> processFeed url addedOn config) urls

updateAllFeeds :: App ()
updateAllFeeds config = do
  urls <- getFeedUrlsFromDB config
  processFeeds urls config

nothingIfEmpty :: Foldable t => t a -> Maybe (t a)
nothingIfEmpty a = if null a then Nothing else Just a

destroyDB :: App ()
destroyDB (Config {..}) =
  withResource connPool $ \conn -> do
    _ <- justRunQuery conn $ fromString "DROP TABLE IF EXISTS feed_items;"
    justRunQuery conn $ fromString "DROP TABLE IF EXISTS feeds;"

removeFeed :: URL -> App ()
removeFeed url (Config {..}) =
  withResource connPool $ \conn -> do
    _ <- setPragmas conn
    res <- failWith DatabaseError $ query_ conn (fromString $ "SELECT id, url FROM feeds where url = '" ++ url ++ "';") :: IO [(Int, String)]
    when (null res) $ throw $ DatabaseError "Feed not found."
    justRunQuery conn $ fromString $ "DELETE FROM feeds where url = '" ++ url ++ "';"

listFeeds :: App [[String]]
listFeeds (Config {..}) = do
  withResource connPool $ \conn -> do
    failWith DatabaseError $ query_ conn selectAllFeeds :: IO [[String]]

setPragmas :: Connection -> IO ()
setPragmas = flip execute_ (fromString "PRAGMA foreign_keys = ON;")

userConfirmation :: String -> IO Bool
userConfirmation msg = do
  putStrLn msg
  putStr "Type 'y' and hit enter to confirm. Any other key to abort: "
  hFlush stdout
  input <- getLine
  pure $ input == "y" || input == "Y"

runMigrations :: Connection -> IO ()
runMigrations conn = do
  let addCreatedAtToFeeds = fromString "ALTER TABLE feeds ADD COLUMN created_at DATETIME DEFAULT CURRENT_TIMESTAMP;" :: Query
      addCreateAtToFeedItems = fromString "ALTER TABLE feed_items ADD COLUMN created_at DATETIME DEFAULT CURRENT_TIMESTAMP;" :: Query
      updateCreatedAtFeeds = fromString "UPDATE feeds SET created_at = CURRENT_TIMESTAMP where created_at is null" :: Query
      updateCreatedAtFeedItems = fromString "UPDATE feed_items SET created_at = CURRENT_TIMESTAMP where created_at is null" :: Query
  failWith DatabaseError $ execute_ conn $ fromString "PRAGMA foreign_keys = ON;"
  failWith DatabaseError $ execute_ conn addCreatedAtToFeeds
  failWith DatabaseError $ execute_ conn addCreateAtToFeedItems
  failWith DatabaseError $ execute_ conn updateCreatedAtFeeds
  failWith DatabaseError $ execute_ conn updateCreatedAtFeedItems

createDigest :: Day -> App [FeedItem]
createDigest day config =
  withResource (connPool config) $ \conn -> do
    let query = fromString $ "SELECT title, link, updated FROM feed_items where created_at = '" ++ show day ++ "' order by updated DESC;" :: Query
    failWith DatabaseError $ query_ conn query :: IO [FeedItem]

logFeedItem :: FeedItem -> IO ()
logFeedItem FeedItem {..} = putStrLn $ title ++ "(" ++ fromMaybe "" link ++ ")"

feedItemsToHtml :: [FeedItem] -> String
feedItemsToHtml items = "<ul>" ++ concatMap (\item -> "<li>" ++ feedItemToHtmlLink item ++ "</li>") items ++ "</ul>"
  where
    feedItemToHtmlLink :: FeedItem -> String
    feedItemToHtmlLink FeedItem {..} =
      "<div><a href='" ++ fromMaybe "#" link ++ "'>" ++ title ++ "</a> </div><div class=\"domain\">" ++ maybe "" showDay updated ++ " &bull; " ++ getDomain link ++ "</div>"

writeDigest :: String -> Day -> [FeedItem] -> IO String
writeDigest template day items = do
  let filename = "digest-" ++ show day ++ ".html"
      digestContents = replaceDigestSummary ("You have " ++ show (length items) ++ " posts to catch up on.") . replaceDigestContent (feedItemsToHtml items) . replaceDigestTitle ("Digest for " ++ showDay day ++ ":") $ template
  failWith DigestError $ writeFile filename digestContents
  pure filename

getDomain :: Maybe String -> String
getDomain url =
  let maybeURI = url >>= parseURI >>= uriAuthority
   in maybe "" uriRegName maybeURI

replaceDigestContent :: String -> String -> String
replaceDigestContent = replaceContent "{digestContent}"

replaceDigestTitle :: String -> String -> String
replaceDigestTitle = replaceContent "{digestTitle}"

replaceDigestSummary :: String -> String -> String
replaceDigestSummary = replaceContent "{digestSummary}"

replaceContent :: String -> String -> String -> String
replaceContent pattern replaceWith content = Data.Text.unpack $ replace (pack pattern) (pack replaceWith) (pack content)

showDay :: Day -> String
showDay day = formatTime defaultTimeLocale "%B %d, %Y" day