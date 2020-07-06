{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Application.Products as APP
import qualified Adapter.Postgres.Config.PostgresConfig as DB
import qualified Adapter.Postgres.Migration.PostgresMigration as M
import qualified Adapter.Postgres.Products as ADA
import qualified Adapter.Postgres.Util.PostgresUtil as U
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, fromMaybe)
import System.Environment (lookupEnv)

import Data.Text (Text)
import Servant
import Adapter.Http.Products
import Data.Proxy
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

proxy :: Proxy ProductApi
proxy = Proxy

server :: Application
server = serve proxy api

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

main :: IO ()
main = do
  conf <- load
  pool <- DB.create conf
  dir <- lookupEnv "DB_MIGRATION_DIR"
  migration <- M.migrate pool (fromMaybe "db/migrations" dir)
  all <- APP.findAll (ADA.findAll pool)
  print all
--main = run 8080 server

  
