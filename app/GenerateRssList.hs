{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}
module GenerateRssList (getFeedLinks) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException (SomeException), try)
import Data.Functor.Identity (Identity)
import Data.List (foldl', intercalate, isInfixOf)
import FetchChannelsList (fetchSubscribedChannels)
import Text.HTML.Scalpel (ScraperT, TagName (..), URL, attr, match, scrapeURL, text, (@:))

rssScraper :: ScraperT String Identity String
rssScraper = attr "href" rssSelector
  where
    rssSelector = TagString "link" @: [match (\_ val -> isInfixOf "feeds/video" val)]

data Feed = Feed URL String | NoFeed URL String deriving (Show)

scrapeUrlToInfo :: MVar () -> String -> IO Feed
scrapeUrlToInfo lock url = do
  -- acquire the lock
  takeMVar lock
  putStrLn $ "Scraping: " ++ url
  res <- try $ scrapeURL url rssScraper :: IO (Either SomeException (Maybe String))
  -- release the lock
  putMVar lock ()
  pure $ case res of
    Left err -> NoFeed url (show err)
    Right maybeInfo -> case maybeInfo of
      Nothing -> NoFeed url "No feed found in the link."
      Just link -> Feed url link

-- generate :: IO ()
-- generate = do
--   urls <- fetchSubscribedChannels

getFeedLinks :: IO (Either String ())
getFeedLinks = do
  urls <- fetchSubscribedChannels
  case urls of
    Left err -> pure $ Left err
    Right _urls -> do
      lock <- newMVar ()
      feeds <- mapConcurrently (scrapeUrlToInfo lock) _urls
      writeFeedsToFile feeds

writeFeedsToFile :: [Feed] -> IO (Either String ())
writeFeedsToFile feeds = do
  let lines = map feedToLine feeds
  result <- try $ writeFile "./feeds.csv" (unlines lines) :: IO (Either SomeException ())
  pure $ case result of
    Left err -> Left $ show err
    Right _ -> Right ()

feedToLine :: Feed -> String
feedToLine (NoFeed url err) = intercalate "," [url, err]
feedToLine (Feed url link) = intercalate "," [url, link]
