{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Application.Products as SERVICE
import qualified Adapter.Http.Products as HTTP
import qualified Adapter.Postgres.Config.PostgresConfig as DB
import qualified Adapter.Postgres.Migration.PostgresMigration as M
import qualified Adapter.Postgres.Products as REPO
import Data.Maybe (fromMaybe)
import Data.Proxy
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (lookupEnv)
import Katip
import Adapter.Katip.Logger (logger)

main :: IO ()
main = do
  pool <- DB.create
  dir <- lookupEnv "DB_MIGRATION_DIR"
  port' <- lookupEnv "APP_PORT" 
    >>= \case
          Just p  -> return (read p :: Int)
          Nothing -> return 8080
  migration <- M.migrate pool (fromMaybe "db/migrations" dir)
  let f1 = SERVICE.findAll (REPO.findAll pool)
      f2 = SERVICE.findById (REPO.findById pool)
      f3 = SERVICE.create (REPO.create pool)
      server = serve proxy $ HTTP.routes f1 f2 f3
  logger $ \logEnv -> do
    runKatipContextT logEnv () "server-start" $ do
      $(logTM) InfoS "Start Server..."
  run port' server
  
proxy :: Proxy HTTP.ProductRoute
proxy = Proxy

--server :: Application