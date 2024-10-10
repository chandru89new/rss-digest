module FetchChannelsList (fetchSubscribedChannels) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException (SomeException), try)
import Control.Monad (liftM)
import Data.Functor.Identity (Identity)
import GHC.IO (liftIO)
import Text.HTML.Scalpel

titleScraper :: ScraperT String IO String
titleScraper = text $ tagSelector "title"

readHtmlSource = try $ readFile "./source.html" :: IO (Either SomeException String)

channelLinkScraper :: Monad m => ScraperT String m [String]
channelLinkScraper = attrs "href" selector
  where
    anchorSelector = TagString "a" @: [AttributeString "id" @= "main-link"]
    divSelector = TagString "div" @: [AttributeString "id" @= "info-section"]
    selector = atDepth (divSelector // anchorSelector) 1

writeChannelsToFile :: [String] -> IO ()
writeChannelsToFile channels = writeFile "./channels.txt" content
  where
    content = unlines (map ("https://youtube.com" ++) channels)

fetchSubscribedChannels :: IO (Either String [String])
fetchSubscribedChannels = do
  contents <- readHtmlSource
  pure $ case contents of
    Left err -> Left $ show err
    Right c -> do
      scrapeResult <- scrapeStringLikeT c channelLinkScraper
      case scrapeResult of
        Nothing -> Left "No content."
        Just v -> Right $ map ("https://youtube.com" ++) v
