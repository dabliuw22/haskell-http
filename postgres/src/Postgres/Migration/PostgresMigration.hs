{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Postgres.Migration.PostgresMigration
  ( migrate,
    MigrationException (..),
    Exception (..),
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (Exception, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as M
import Envs.Env (FromEnv (..))
import Katip (Severity (ErrorS), logStr, logTM, runKatipContextT)
import Logger.Log (logger)
import System.Environment (lookupEnv)

newtype MgConfig = MgConfig
  { dir :: String
  }
  deriving (Eq)

instance FromEnv MgConfig where
  fromEnv = do
    dir' <- lookupEnv "DB_MIGRATION_DIR"
    return (MgConfig (fromMaybe "db/migrations" dir'))

migrate ::
  (MonadIO m, MonadThrow m) =>
  Pool PG.Connection ->
  m ()
migrate pool = do
  dir' <- liftIO fromEnv
  migration <-
    liftIO $
      withResource
        pool
        (\conn -> PG.withTransaction conn (M.runMigrations False conn (commands dir')))
  case migration of
    M.MigrationError e -> do
      liftIO $
        logger $ \logEnv -> do
          runKatipContextT logEnv () namespace $ do
            $(logTM) ErrorS $ logStr ("Migration Error: " ++ show e)
      throwM MigrationException
    _ -> return ()
  where
    commands :: MgConfig -> [M.MigrationCommand]
    commands (MgConfig dir') = [M.MigrationInitialization, M.MigrationDirectory dir']
    namespace = "migrations"

data MigrationException = MigrationException deriving (Show, Typeable)

instance Exception MigrationException
