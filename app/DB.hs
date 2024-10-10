{-# LANGUAGE RecordWildCards #-}

module DB (initializeTables, justRunQuery) where

import App (AppM, Config (Config), liftEitherAppM)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, throw, try)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ask)
import Data.Either (fromRight)
import Data.Pool (withResource)
import Data.String (IsString (fromString))
import Database.SQLite.Simple
import FeedItemExtractor

dbFile = "./feeds.db"

-- SQL query to create the `feed_items` table if it doesn't already exist
createFeedItemsTable :: Query
createFeedItemsTable =
  fromString
    "CREAT TABLE IF NOT EXISTS feed_items (\
    \ title TEXT NOT NULL, \
    \ link TEXT NOT NULL PRIMARY KEY, \
    \ updated DATETIME NOT NULL \
    \);"

-- SQL query to create the `feeds` table if it doesn't already exist
createFeedsTable :: Query
createFeedsTable =
  fromString
    "CREATE TABLE IF NOT EXISTS feeds (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \
    \ url TEXT NOT NULL, \
    \ site_name TEXT NOT NULL \
    \);"

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

initDB :: AppM ()
initDB = do
  Config connPool <- ask
  result <- liftIO $ withResource connPool $ runExceptT . initializeTables
  liftEitherAppM result

-- insertFeedItem :: Connection -> FeedItem -> ExceptT SomeException IO ()
insertFeedItem FeedItem {..} = do
  let query = fromString "INSERT INTO feed_items (title, link, updated) VALUES (?, ?, ?);"
  conn <- openConn
  ExceptT $ try $ execute conn query $ toRow (title, link, updated)

withConn action = do
  conn <- openConn
  action
  liftIO $ close conn

runQueryAction queryAction = do
  conn <- openConn
  queryAction conn
  liftIO $ close conn

openConn :: ExceptT SomeException IO Connection
openConn = ExceptT $ do
  try $ open "./feeds.db"
