{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Adapter.Postgres.Util.PostgresUtil (PostgresUtil(..), PostgresException(..)) where

import Control.Exception (Exception, catch)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool
import Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

class PostgresUtil m where
  queryOne :: (FromRow a, ToRow b) => Pool PG.Connection
    -> PG.Query -> b -> m (Maybe a)
  queryListWithoutParams :: FromRow a => Pool PG.Connection
    -> PG.Query -> m [a]
  queryList :: (FromRow a, ToRow b) => Pool PG.Connection
    -> PG.Query -> b -> m [a]
  command :: ToRow b => Pool PG.Connection -> PG.Query
    -> b -> m ()

instance PostgresUtil IO where
  queryOne pool q b = queryOne' pool q b
  queryListWithoutParams pool q = queryListWithoutParams' pool q
  queryList pool q b = queryList' pool q b
  command pool c = command' pool c

queryOne' :: (MonadIO m, MonadThrow m, FromRow a, ToRow b)
  => Pool PG.Connection
  -> PG.Query -> b -> m (Maybe a)
queryOne' p' q' b = do
  result <- liftIO $ withResource p' (\conn -> PG.query conn q' b)
    `catch` handleSqlError
  case result of
        (h: _) -> return $ Just h
        _      -> return Nothing

queryList' :: (MonadIO m, MonadThrow m, FromRow a, ToRow b)
  => Pool PG.Connection
  -> PG.Query -> b -> m [a]
queryList' p' q' b = liftIO $ withResource p' (\conn -> PG.query conn q' b)
  `catch` handleSqlError

queryListWithoutParams' :: (MonadIO m, MonadThrow m, FromRow a)
  => Pool PG.Connection
  -> PG.Query -> m [a]
queryListWithoutParams' p' q' = liftIO $ withResource p' (`PG.query_` q')
  `catch` handleSqlError

command' :: (MonadIO m, MonadThrow m, ToRow b)
  => Pool PG.Connection
  -> PG.Query -> b -> m ()
command' p' q' b = do
  result <- liftIO $ withResource p' (\conn -> PG.execute conn q' b)
    `catch` handleSqlError
  return ()

newtype PostgresException = PostgresException String deriving (Show, Typeable)

instance Exception PostgresException

handleSqlError :: (MonadIO m, MonadThrow m) => PG.SqlError -> m a
handleSqlError e @ (PG.SqlError _ _ m _ _) = throwM $ PostgresException (show e)