{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Adapter.Http.Products as HTTP
import Adapter.Katip.Logger (logger)
import qualified Adapter.Postgres.Config.PostgresConfig as DB
import qualified Adapter.Postgres.Migration.PostgresMigration as M
import qualified Adapter.Postgres.Products as REPO
import qualified Application.Products as SERVICE
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Katip (Severity (InfoS), logTM, runKatipContextT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( cors,
    corsMethods,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
  )
import Servant (Application, Proxy (..), serve)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  pool <- DB.create
  dir <- lookupEnv "DB_MIGRATION_DIR"
  port' <-
    lookupEnv "APP_PORT"
      >>= \case
        Just p -> return (read p :: Int)
        Nothing -> return 8080
  _ <- M.migrate pool (fromMaybe "db/migrations" dir)
  let f1 = SERVICE.findAll (REPO.findAll pool)
      f2 = SERVICE.findById (REPO.findById pool)
      f3 = SERVICE.create (REPO.create pool)
      f4 = SERVICE.deleteById (REPO.deleteById pool)
      f5 = SERVICE.update (REPO.update pool)
      server = serve proxy $ routes f1 f2 f3 f4 f5
  logger $ \logEnv -> do
    runKatipContextT logEnv () "server-start" $ do
      $(logTM) InfoS "Start Server..."
  run port' $ corsMiddleware server -- run port' $ simpleCors server

type API = HTTP.ProductRoute

routes = HTTP.routes

proxy :: Proxy API
proxy = Proxy

corsMiddleware :: Application -> Application
corsMiddleware =
  cors
    ( const $
        Just
          ( simpleCorsResourcePolicy
              { corsMethods = ["DELETE", "GET", "OPTIONS", "PATCH", "POST", "PUT"],
                corsRequestHeaders = ["Authorization", "Cache-Control", "Content-Type"]
              }
          )
    )

--server :: Application
