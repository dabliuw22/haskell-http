{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Adapter.Postgres.Util.PostgresUtil (PostgresUtil(..), PostgresException(..)) where

import Adapter.Katip.Logger
import Control.Exception (Exception, catch)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool
import Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Katip
import Data.Int (Int64)

class PostgresUtil m where
  queryOne :: (FromRow a, ToRow b) => Pool PG.Connection
    -> PG.Query -> b -> m (Maybe a)
  queryListWithoutParams :: FromRow a => Pool PG.Connection
    -> PG.Query -> m [a]
  queryList :: (FromRow a, ToRow b) => Pool PG.Connection
    -> PG.Query -> b -> m [a]
  command :: ToRow b => Pool PG.Connection -> PG.Query
    -> b -> m (Bool)

instance PostgresUtil IO where
  queryOne pool q b = do
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS $ "Query One Start..."
    result <- queryOne' pool q b `catch` \e -> handleSqlError e namespace
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS $ "Query One End..."
    return result
    where
      namespace = "query-one"
  queryListWithoutParams pool q = do
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS $ "Query List Without Params Start..."
    result <- queryListWithoutParams' pool q `catch` \e -> handleSqlError e namespace
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS $ "Query List Without Params End..."
    return result
    where
      namespace = "query-list-without-params"
  queryList pool q b = do
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS $ "Query List Start..."
    result <- queryList' pool q b `catch` \e -> handleSqlError e namespace
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS $ "Query List End..."
    return result
    where
      namespace = "query-list"
  command pool c b = do
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS $ "Command Start..."
    result <- command' pool c b `catch` \e -> handleSqlError e namespace
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS $ "Command End..."
    return result
    where
      namespace = "command"

queryOne' :: (MonadIO m, MonadThrow m, FromRow a, ToRow b)
  => Pool PG.Connection
  -> PG.Query -> b -> m (Maybe a)
queryOne' p' q' b = do
  result <- liftIO $ withResource p' (\conn -> PG.query conn q' b)
  case result of
        (h: _) -> return $ Just h
        _      -> return Nothing

queryList' :: (MonadIO m, MonadThrow m, FromRow a, ToRow b)
  => Pool PG.Connection
  -> PG.Query -> b -> m [a]
queryList' p' q' b = liftIO $ withResource p' (\conn -> PG.query conn q' b)

queryListWithoutParams' :: (MonadIO m, MonadThrow m, FromRow a)
  => Pool PG.Connection
  -> PG.Query -> m [a]
queryListWithoutParams' p' q' = liftIO $ withResource p' (`PG.query_` q')

command' :: (MonadIO m, MonadThrow m, ToRow b)
  => Pool PG.Connection
  -> PG.Query -> b -> m Bool
command' p' q' b = do
  result <- liftIO $ withResource p' (\conn -> PG.execute conn q' b)
  return (result > 0)

newtype PostgresException = PostgresException String deriving (Show, Typeable)

instance Exception PostgresException

handleSqlError :: PG.SqlError -> Namespace -> IO a
handleSqlError e @ (PG.SqlError _ _ m _ _) namespace = do
  logger $ \logEnv -> do
        runKatipContextT logEnv () namespace $ do
          $(logTM) ErrorS $ "Error..."
  throwM $ PostgresException (show e)