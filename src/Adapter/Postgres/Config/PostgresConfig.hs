module Adapter.Postgres.Config.PostgresConfig (PgConfig(..), create, destroy) where

import qualified Database.PostgreSQL.Simple as PG
import Data.Pool
import Control.Monad.IO.Class (MonadIO, liftIO)

data PgConfig =
   PgConfig {
    host :: String,
    port :: Integer,
    user :: String,
    pass :: String,
    database :: String
   } deriving Eq
   
create :: MonadIO m => PgConfig -> m (Pool PG.Connection)
create conf = liftIO $ createPool (connection conf) close 1 10 15

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