{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Application.Products as SERVICE
import qualified Adapter.Http.Products as HTTP
import qualified Adapter.Postgres.Config.PostgresConfig as DB
import qualified Adapter.Postgres.Migration.PostgresMigration as M
import qualified Adapter.Postgres.Products as REPO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (lookupEnv)

main :: IO ()
main = do
  conf <- load
  pool <- DB.create conf
  dir <- lookupEnv "DB_MIGRATION_DIR"
  migration <- M.migrate pool (fromMaybe "db/migrations" dir)
  let f1 = SERVICE.findAll (REPO.findAll pool)
      f2 = SERVICE.findById (REPO.findById pool)
      f3 = SERVICE.create (REPO.create pool)
      server = serve proxy $ HTTP.routes f1 f2 f3
  run 8080 server
  
proxy :: Proxy HTTP.ProductRoute
proxy = Proxy

--server :: Application

load :: MonadIO m => m DB.PgConfig
load = do
  host' <- liftIO $ lookupEnv "DB_HOST"
  port' <- liftIO $ lookupEnv "DB_PORT"
  user' <- liftIO $ lookupEnv "DB_USER"
  pass' <- liftIO $ lookupEnv "DB_PASS"
  name' <- liftIO $ lookupEnv "DB_NAME"
  return DB.PgConfig {
           DB.host = fromMaybe "localhost" host',
           DB.port = read (fromMaybe "5432" port') :: Integer,
           DB.user = fromMaybe "haskell" user',
           DB.pass = fromMaybe "haskell" pass',
           DB.database = fromMaybe "haskell_db" name'
         }