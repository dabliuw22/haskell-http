module Adapter.Postgres.Config.PostgresConfig (PgConfig(..), create, destroy) where

import qualified Database.PostgreSQL.Simple as PG
import Data.Maybe (fromMaybe)
import Data.Pool
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Environment (lookupEnv)

data PgConfig =
   PgConfig {
    host :: String,
    port :: Integer,
    user :: String,
    pass :: String,
    database :: String
   } deriving Eq

loadPgConfig :: MonadIO m => m PgConfig
loadPgConfig = do
  host' <- liftIO $ lookupEnv "DB_HOST"
  port' <- liftIO $ lookupEnv "DB_PORT"
  user' <- liftIO $ lookupEnv "DB_USER"
  pass' <- liftIO $ lookupEnv "DB_PASS"
  name' <- liftIO $ lookupEnv "DB_NAME"
  return PgConfig {
           host = fromMaybe "localhost" host',
           port = read (fromMaybe "5432" port') :: Integer,
           user = fromMaybe "haskell" user',
           pass = fromMaybe "haskell" pass',
           database = fromMaybe "haskell_db" name'
         }
   
create :: MonadIO m => m (Pool PG.Connection)
create = do
  conf <- loadPgConfig
  liftIO $ createPool (connection conf) close 1 10 15

destroy :: MonadIO m => Pool PG.Connection -> m ()
destroy pool = liftIO $ destroyAllResources pool

connection :: MonadIO m => PgConfig -> m PG.Connection
connection conf = liftIO $ PG.connect PG.ConnectInfo {
  PG.connectHost = host conf,
  PG.connectPort = fromIntegral (port conf),
  PG.connectUser = user conf,
  PG.connectPassword = pass conf,
  PG.connectDatabase = database conf
}

close :: MonadIO m => PG.Connection -> m ()
close conn = liftIO $ PG.close conn