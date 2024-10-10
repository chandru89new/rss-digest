module App (AppM, Config (..), runApp, liftEitherAppM) where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExcept, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Pool (Pool, createPool, defaultPoolConfig, newPool)
import Data.Pool.Introspection (PoolConfig)
import Database.SQLite.Simple (Connection, close, open)

type AppM a = ReaderT Config (ExceptT SomeException IO) a

newtype Config = Config
  { connPool :: Pool Connection
  }

poolConfig :: PoolConfig a
poolConfig = undefined

runApp app = do
  pool <- newPool (defaultPoolConfig (open "./feeds.db") close 60.0 10)
  let config = Config {connPool = pool}
  runAppM config app

runAppM :: Config -> AppM a -> IO (Either SomeException a)
runAppM config app = do
  runExceptT $ runReaderT app config

liftEitherAppM :: Either SomeException a -> AppM a
liftEitherAppM = lift . either throwE return
