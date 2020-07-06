module Adapter.Postgres.Util.PostgresUtil (PostgresUtil(..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool
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
  queryList pool q b = queryList pool q b
  command pool c = undefined

queryOne' :: (MonadIO m, FromRow a, ToRow b) => Pool PG.Connection 
  -> PG.Query -> b -> m (Maybe a)
queryOne' p' q' b = do
  result <- liftIO $ withResource p' (\conn -> PG.query conn q' b)
  case result of
        (h: _) -> return $ Just h
        _      -> return Nothing

queryList' :: (MonadIO m, FromRow a, ToRow b) => Pool PG.Connection 
  -> PG.Query -> b -> m [a]
queryList' p' q' b = liftIO $ withResource p' (\conn -> PG.query conn q' b)

queryListWithoutParams' :: (MonadIO m, FromRow a) => Pool PG.Connection 
  -> PG.Query -> m [a]
queryListWithoutParams' p' q' = liftIO $ withResource p' (`PG.query_` q')

command' :: (MonadIO m, ToRow b) =>  Pool PG.Connection 
  -> PG.Query -> b -> m ()
command' p' q' b = liftIO $ withResource p' (\conn -> PG.execute conn q' b) 
  >>= (\r -> return ())