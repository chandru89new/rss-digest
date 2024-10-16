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
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.String (IsString (fromString))
import Data.Text (Text, pack, replace, strip, unpack)
import Data.Text.Encoding (decodeUtf8)
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
  main' command

main' :: Command -> IO ()
main' command =
  case command of
    AddFeed link -> do
      let url = parseURL link
      case url of
        Just _url -> do
          runApp $ \config -> do
            res <- try $ insertFeed _url config
            either showAppError (const $ putStrLn "Added.") res
            when (isRight res) $ processFeeds (fromRight [] res) config
        Nothing -> putStrLn "I am not able to parse the URL. Please provide a valid URL."
    RemoveFeed url -> runApp $ \config -> do
      input <- userConfirmation "This will remove the feed and all the posts associated with it."
      if input
        then do
          removeFeed url config
        else putStrLn "I have cancelled it."
    ListFeeds -> runApp $ \config -> do
      feeds <- listFeeds config >>= pure . concat
      putStrLn $ intercalate "\n" feeds
    RefreshFeed url -> do
      runApp $ \config -> do
        refreshFeed url config
        putStrLn "I have refreshed the feed."
    RefreshFeeds -> do
      runApp $ \config -> do
        updateAllFeeds config
        putStrLn "I have refreshed all the feeds."
    CreateTodayDigest -> do
      today <- fmap utctDay getCurrentTime
      runApp $ \config@Config {..} -> do
        items <- createDigest (today, today) config
        if null items
          then putStrLn $ "I can't create a digest for " ++ showDay today ++ ". There are no posts to make a digest out of."
          else do
            putStrLn $ "I am now preparing digest for " ++ showDay today ++ ". Give me a few seconds..."
            file <- writeDigest template (today, today) items
            putStrLn $ "I have saved the digest file at " ++ file ++ "."
    CreateRangeDigest argPairs -> do
      from <- pure $ extractArgString "--from" argPairs >>= parseTimeM True defaultTimeLocale "%Y-%m-%d" :: IO (Maybe Day)
      to <- pure $ extractArgString "--to" argPairs >>= parseTimeM True defaultTimeLocale "%Y-%m-%d" :: IO (Maybe Day)
      runApp $ \config@Config {..} -> case (from, to) of
        (Just s, Just e) -> do
          items <- createDigest (s, e) config
          if null items
            then putStrLn "There are not posts in the given range to make a digest."
            else do
              putStrLn $ "I am now preparing digest for " ++ showDay s ++ " to " ++ showDay e ++ "..."
              file <- writeDigest template (s, e) items
              putStrLn $ "I have saved the digest file at " ++ file ++ "."
        (_, _) -> showAppError $ ArgError "I couldn't understand the date range values. Provide a valid date range in the YYYY-MM-DD format. Type 'rdigest help' for more information."
    PurgeEverything -> do
      input <- userConfirmation "This will remove all feeds and all the posts associated with them."
      if input
        then do
          putStrLn "Nuking everything..."
          _ <- runApp destroyDB
          putStrLn "Fin."
        else putStrLn "I have cancelled it."
    InvalidCommand -> putStrLn progHelp

progHelp :: String
progHelp =
  "Usage: rdigest <command> [args]\n\
  \Commands:\n\
  \  help - Show this help.\n\
  \  add <url> - Add a feed. <url> must be valid HTTP(S) URL.\n\
  \  remove <url> - Remove a feed and all its associated posts with the given url.\n\
  \  digest - Generate the digest for today.\n\
  \  digest --from <start_date> --to <end_date> - Generate the digest for a given date range. Dates in the YYYY-MM-DD format.\n\
  \  list feeds - List all feeds\n\
  \  refresh - Refresh all feeds\n\
  \  purge - Purge everything\n"

data Command
  = AddFeed URL
  | RefreshFeed URL
  | RefreshFeeds
  | ListFeeds
  | RemoveFeed URL
  | CreateTodayDigest
  | PurgeEverything
  | InvalidCommand
  | CreateRangeDigest [ArgPair]
  deriving (Eq)

getCommand :: IO Command
getCommand = do
  args <- getArgs
  pure $ case args of
    ("add" : url : _) -> AddFeed url
    ("remove" : url : _) -> RemoveFeed url
    ("list" : "feeds" : _) -> ListFeeds
    ("refresh" : url : _) -> RefreshFeed url
    ["refresh"] -> RefreshFeeds
    ["digest"] -> CreateTodayDigest
    ("digest" : xs) -> CreateRangeDigest $ groupCommandArgs xs
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
  pure $ (Data.Text.unpack . replaceSmartQuotes . decodeUtf8 . getResponseBody) res

type App a = Config -> IO a

data AppError
  = FetchError String
  | DatabaseError String
  | FeedParseError String
  | ArgError String
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
  either showAppError (const $ return ()) res

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
parseDate datetime = fmap utctDay $ firstJust $ map tryParse [fmt1, fmt2, fmt3, fmt4, fmt5, fmt6]
  where
    fmt1 = "%Y-%m-%dT%H:%M:%S%z"
    fmt2 = "%a, %d %b %Y %H:%M:%S %z"
    fmt3 = "%a, %d %b %Y %H:%M:%S %Z"
    fmt4 = "%Y-%m-%dT%H:%M:%S%Z"
    fmt5 = "%Y-%m-%dT%H:%M:%S%Q%z"
    fmt6 = "%Y-%m-%dT%H:%M:%S%Q%Z"
    tryParse fmt = parseTimeM True defaultTimeLocale fmt datetime :: Maybe UTCTime
    firstJust :: [Maybe a] -> Maybe a
    firstJust xs = go xs Nothing
      where
        go [] acc = acc
        go (x : xs_) acc = case x of
          Just y -> Just y
          Nothing -> go xs_ acc

dbFile :: String
dbFile = "./digest.db"

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

newtype FeedId = FeedId Int

instance FromRow FeedId where
  fromRow = FeedId <$> field

insertFeed :: URL -> App [(Int, URL)]
insertFeed feedUrl (Config {..}) = do
  let q = fromString $ "INSERT INTO feeds (url) VALUES ('" ++ feedUrl ++ "');"
  withResource connPool $ \conn -> do
    _ <- failWith DatabaseError $ execute_ conn q
    failWith DatabaseError $ query_ conn (fromString $ "SELECT id, url FROM feeds where url = '" ++ feedUrl ++ "';")

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
  _ <- liftIO $ putStrLn $ "Processing: " ++ url
  _ <- withResource connPool $ \conn -> do
    feedIdExists <- failWith DatabaseError $ query_ conn (fromString $ "SELECT id FROM feeds where id = " ++ show feedId ++ ";") :: IO [FeedId]
    when (null feedIdExists) $ throw $ DatabaseError "You have to first add this feed to your database. Try `rdigest add <url>`."
  contents <- fetchUrl url
  let feedItems = extractFeedItems contents
      unwrappedFeedItems = fromMaybe [] feedItems
  when (isNothing feedItems) $ putStrLn $ "I couldn't find anything on: " ++ url ++ "."
  res <-
    ( try $ failWith DatabaseError $ withResource connPool $ \conn -> do
        _ <- setPragmas conn
        mapM (handleInsert conn) unwrappedFeedItems
    ) ::
      IO (Either AppError [Int])
  when ((not . null) unwrappedFeedItems && isRight res) $ liftIO $ putStrLn $ "Finished processing " ++ url ++ ". I discovered " ++ show (length unwrappedFeedItems) ++ " posts. I have added " ++ show (sum $ fromRight [] res) ++ " posts to the database (duplicates are ignored)."
  where
    handleInsert :: Connection -> FeedItem -> IO Int
    handleInsert conn feedItem = do
      res <- (try $ insertFeedItem conn (feedId, addedOn, feedItem)) :: IO (Either AppError (Maybe FeedItem))
      when (isLeft res) $ print (fromLeft (DatabaseError "I ran into an error when trying to save a feed to the database.") res)
      pure $ either (const 0) (\r -> if isJust r then 1 else 0) res

processFeeds :: [(Int, URL)] -> App ()
processFeeds urls config = do
  utcTime <- getCurrentTime
  let addedOn = utctDay utcTime
  mapM_
    ( \url -> do
        res <- (try :: IO a -> IO (Either AppError a)) $ processFeed url addedOn config
        case res of
          Left e -> showAppError e
          Right _ -> pure ()
    )
    urls

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
    when (null res) $ throw $ DatabaseError "I could not find any such feed in the database. Maybe it's already gone?"
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

type FeedItemWithId = (URL, FeedItem)

createDigest :: (Day, Day) -> App [(URL, [FeedItem])]
createDigest (day1, day2) config = withResource (connPool config) $ \conn -> do
  xs <- failWith DatabaseError $ query_ conn query :: IO [(String, String, String, Day)]
  pure $ groupByFeedUrl xs
  where
    query = fromString $ "select feeds.url, f.link, f.title, f.updated from feed_items f join feeds on f.feed_id = feeds.id where f.updated >= '" ++ show day1 ++ "' and f.updated <= '" ++ show day2 ++ "' order by updated DESC;"
    groupByFeedUrl :: [(String, String, String, Day)] -> [(URL, [FeedItem])]
    groupByFeedUrl = foldr go' []
      where
        go' (url, link, title, updated) acc = case lookup url acc of
          Just xs -> (url, FeedItem {title = title, link = Just link, updated = Just updated} : xs) : filter ((/= url) . fst) acc
          Nothing -> (url, [FeedItem {title = title, link = Just link, updated = Just updated}]) : acc

logFeedItem :: FeedItem -> IO ()
logFeedItem FeedItem {..} = putStrLn $ title ++ "(" ++ fromMaybe "" link ++ ")"

feedItemsToHtml :: [FeedItem] -> String
feedItemsToHtml items = "<ul>" ++ concatMap (\item@FeedItem {..} -> "<a class=\"li\" href=\"" ++ fromMaybe "" link ++ "\"><li>" ++ feedItemToHtmlLink item ++ "</li></a>") items ++ "</ul>"
  where
    feedItemToHtmlLink :: FeedItem -> String
    feedItemToHtmlLink FeedItem {..} =
      "<div class=\"title\">" ++ title ++ "</div><div class=\"domain\">" ++ getDomain link ++ " &bull; " ++ maybe "" showDay updated ++ "</div>"

writeDigest :: String -> (Day, Day) -> [(URL, [FeedItem])] -> IO String
writeDigest template (day1, day2) items = do
  let filename = "digest-" ++ show day2 ++ ".html"
  failWith DigestError $ writeFile filename (generateDigestContent items)
  pure filename
  where
    generateDigestContent :: [(URL, [FeedItem])] -> String
    generateDigestContent xs =
      let titleReplaced = replaceDigestTitle ("Digest — " ++ dateRange ++ ":") template
          summaryReplaced = replaceDigestSummary ("You have " ++ show (length $ concatMap snd xs) ++ " posts to catch up on.") titleReplaced
          contentReplaced = replaceDigestContent (concatMap convertGroupToHtml xs) summaryReplaced
       in contentReplaced

    dateRange :: String
    dateRange = if day1 == day2 then wrapDate (showDay day2) else wrapDate (showDay day1) ++ " to " ++ wrapDate (showDay day2)

    wrapDate :: String -> String
    wrapDate date = "<span class=\"digest-date\">" ++ date ++ "</span>"

    convertGroupToHtml :: (URL, [FeedItem]) -> String
    convertGroupToHtml (url, xs) = "<details open><summary><h2 class=\"summary\">" ++ url ++ "</h2> (" ++ show (length xs) ++ ")</summary><p>" ++ feedItemsToHtml xs ++ "</p></details>"

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

replaceSmartQuotes :: Text -> Text
replaceSmartQuotes =
  replace (pack "“") (pack "\"")
    . replace (pack "”") (pack "\"")
    . replace (pack "‘") (pack "'")
    . replace (pack "’") (pack "'")

type ArgPair = (String, ArgVal)

data ArgVal = ArgBool Bool | ArgString String deriving (Eq, Show)

groupCommandArgs :: [String] -> [ArgPair]
groupCommandArgs = go []
  where
    go :: [ArgPair] -> [String] -> [ArgPair]
    go acc [] = acc
    go acc [x] = (x, ArgBool True) : acc
    go acc (x : y : rest)
      | isFlag x && not (isFlag y) = go ((x, ArgString y) : acc) rest
      | isFlag x && isFlag y = go ((x, ArgBool True) : acc) (y : rest)
      | otherwise = go acc (y : rest)
      where
        isFlag :: String -> Bool
        isFlag str = "--" `isInfixOf` str && length str > 2

{-
if ("--test" : "value" : _) -> acc ++ [("test", "value")]
if ("--test" : x starts with "--" : xs) -> acc ++ [("test", ArgBool True)] ++ go (x:xs)
-}
extractArgString :: String -> [ArgPair] -> Maybe String
extractArgString key argPairs = lookup key argPairs >>= extractString
  where
    extractString :: ArgVal -> Maybe String
    extractString (ArgString str) = Just str
    extractString _ = Nothing

extractArgBool :: String -> [ArgPair] -> Maybe Bool
extractArgBool key argPairs = lookup key argPairs >>= extractBool
  where
    extractBool :: ArgVal -> Maybe Bool
    extractBool (ArgBool bool) = Just bool
    extractBool _ = Nothing

showAppError :: AppError -> IO ()
showAppError (FetchError msg) = putStrLn $ "Error fetching URL: " ++ msg
showAppError (DatabaseError msg) = putStrLn $ "Database error: " ++ msg
showAppError (FeedParseError msg) = putStrLn $ "Error parsing feed: " ++ msg
showAppError (ArgError msg) = putStrLn $ "Argument error: " ++ msg
showAppError (DigestError msg) = putStrLn $ "Digest error: " ++ msg
showAppError (GeneralError msg) = putStrLn $ "Unknown error: " ++ msg

refreshFeed :: URL -> App ()
refreshFeed url config@Config {..} = do
  let query = fromString $ "select id, url from feeds where url = '" ++ url ++ "';"
  res <- withResource connPool $ \conn -> failWith DatabaseError $ query_ conn query :: IO [(Int, String)]
  case res of
    [] -> throw $ DatabaseError $ "I could not find " ++ url ++ " in your list of feeds. Try `rdigest list feeds` to see your feeds."
    (feedId, _) : _ -> do
      processFeeds [(feedId, url)] config