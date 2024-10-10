module FetchURL (fetchUrl) where

import Control.Exception (SomeException (SomeException), try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.ByteString.Char8 (ByteString, unpack)
import Network.HTTP.Simple (Request, Response, getResponseBody, httpBS, parseRequest)

fetchUrl :: String -> ExceptT SomeException IO String
fetchUrl url = ExceptT $ do
  req <- parseRequest url
  res <- try $ httpBS req
  return $ fmap (unpack . getResponseBody) res
