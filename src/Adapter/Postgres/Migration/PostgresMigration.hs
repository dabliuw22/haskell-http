{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Adapter.Postgres.Migration.PostgresMigration where

import Adapter.Katip.Logger
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch
import Data.Pool
import Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as M
import Katip

migrate :: (MonadIO m, MonadThrow m) => Pool PG.Connection -> String -> m ()
migrate pool dir = do
  migration <- liftIO $ withResource pool
    (\conn -> PG.withTransaction conn (M.runMigrations False conn commands))
  case migration of
    M.MigrationError e -> do
      liftIO $ logger $ \logEnv -> do
          runKatipContextT logEnv () namespace $ do
            $(logTM) ErrorS $ logStr ("Migration Error: " ++ show e)
      throwM MigrationException
    _                  -> return ()
  where
    commands = [M.MigrationInitialization, M.MigrationDirectory dir]
    namespace = "migrations"
    
data MigrationException = MigrationException deriving (Show, Typeable)

instance Exception MigrationException