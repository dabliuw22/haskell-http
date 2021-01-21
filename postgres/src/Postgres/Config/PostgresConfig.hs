module Postgres.Config.PostgresConfig
  ( create,
    destroy,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool, destroyAllResources)
import Data.Time.Clock (NominalDiffTime (..))
import qualified Database.PostgreSQL.Simple as PG
import Envs.Env (FromEnv (..))
import System.Environment (lookupEnv)

data PgConfig = PgConfig
  { host :: String,
    port :: Integer,
    user :: String,
    pass :: String,
    database :: String,
    subpools :: Int,
    maxConn :: Int,
    maxIdleTime :: NominalDiffTime
  }
  deriving (Eq)

{-
DB_SUBPOOLS: subpools: the pool can have several independent pools.
DB_MAXIMUN_IDLE_TIME: the maximum idle time of a connection (in seconds) before it is closed.
DB_MAXIMUN_CONNECTIONS: the maximum amount of connections in the pool.
-}
instance FromEnv PgConfig where
  fromEnv = do
    host' <- liftIO $ lookupEnv "DB_HOST"
    port' <- liftIO $ lookupEnv "DB_PORT"
    user' <- liftIO $ lookupEnv "DB_USER"
    pass' <- liftIO $ lookupEnv "DB_PASS"
    name' <- liftIO $ lookupEnv "DB_NAME"
    subpools' <- liftIO $ lookupEnv "DB_SUBPOOLS"
    maxConn' <- liftIO $ lookupEnv "DB_MAXIMUN_CONNECTIONS"
    maxIdle' <-
      liftIO $
        lookupEnv "DB_MAXIMUN_IDLE_TIME"
          >>= \v ->
            return (read (fromMaybe "10" v) :: Integer)
              >>= \v -> return (fromInteger v :: NominalDiffTime)
    return
      PgConfig
        { host = fromMaybe "localhost" host',
          port = read (fromMaybe "5432" port') :: Integer,
          user = fromMaybe "haskell" user',
          pass = fromMaybe "haskell" pass',
          database = fromMaybe "haskell_db" name',
          subpools = read (fromMaybe "1" subpools') :: Int,
          maxConn = read (fromMaybe "15" maxConn') :: Int,
          maxIdleTime = maxIdle'
        }

create :: MonadIO m => m (Pool PG.Connection)
create = do
  conf <- liftIO fromEnv
  liftIO $
    createPool
      (connection conf)
      close
      (subpools conf)
      (maxIdleTime conf)
      (maxConn conf)

destroy :: MonadIO m => Pool PG.Connection -> m ()
destroy pool = liftIO $ destroyAllResources pool

connection :: MonadIO m => PgConfig -> m PG.Connection
connection (PgConfig h p u ps d _ _ _) =
  liftIO $
    PG.connect
      PG.ConnectInfo
        { PG.connectHost = h,
          PG.connectPort = fromIntegral p,
          PG.connectUser = u,
          PG.connectPassword = ps,
          PG.connectDatabase = d
        }

close :: MonadIO m => PG.Connection -> m ()
close conn = liftIO $ PG.close conn
