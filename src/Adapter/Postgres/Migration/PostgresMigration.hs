{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Adapter.Postgres.Migration.PostgresMigration where

import Adapter.Katip.Logger (logger)
import Control.Exception (Exception)
import Control.Monad.Catch (Exception, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool (Pool, withResource)
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as M
import Katip (Severity (ErrorS), logStr, logTM, runKatipContextT)

migrate :: (MonadIO m, MonadThrow m) => Pool PG.Connection -> String -> m ()
migrate pool dir = do
  migration <-
    liftIO $
      withResource
        pool
        (\conn -> PG.withTransaction conn (M.runMigrations False conn commands))
  case migration of
    M.MigrationError e -> do
      liftIO $
        logger $ \logEnv -> do
          runKatipContextT logEnv () namespace $ do
            $(logTM) ErrorS $ logStr ("Migration Error: " ++ show e)
      throwM MigrationException
    _ -> return ()
  where
    commands = [M.MigrationInitialization, M.MigrationDirectory dir]
    namespace = "migrations"

data MigrationException = MigrationException deriving (Show, Typeable)

instance Exception MigrationException