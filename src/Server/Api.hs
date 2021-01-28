{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api (acquire, use, release) where

import qualified Adapter.Http.Products as HTTP
import qualified Adapter.Postgres.Products as REPO
import qualified Application.Products as SERVICE
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Proxy (Proxy (..))
import qualified Database.PostgreSQL.Simple as PG
import Katip (Severity (InfoS), logTM, runKatipContextT)
import Logger.Log (logger)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( cors,
    corsMethods,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
  )
import qualified Postgres.Config.PostgresConfig as DB
import qualified Postgres.Migration.PostgresMigration as M
import Servant
  ( Application,
    Proxy (..),
    Server,
    serve,
    type (:<|>) (..),
  )
import System.Environment (lookupEnv)

newtype Resources = Resources (Pool PG.Connection)

acquire :: IO Resources
acquire = Resources <$> DB.create

use :: Resources -> IO ()
use resource@(Resources pool) = do
  port' <-
    lookupEnv "APP_PORT"
      >>= \case
        Just p -> return (read p :: Int)
        Nothing -> return 8080
  _ <- M.migrate pool
  let server = serve proxy $ api resource
  logger $ \logEnv -> do
    runKatipContextT logEnv () "server-start" $ do
      $(logTM) InfoS "Start Server..."
  run port' $ corsMiddleware server -- run port' $ simpleCors server

release :: Resources -> IO ()
release (Resources pool) = do
  _ <- DB.destroy pool
  logger $ \logEnv -> do
    runKatipContextT logEnv () "server-end" $ do
      $(logTM) InfoS "End Server..."

type API = HTTP.ProductRoute -- :<|> HTTP.OtherRoute

proxy :: Proxy API
proxy = Proxy

api :: Resources -> Server API
api (Resources pool) = productRoute pool -- :<|> otherRoute pool

productRoute :: Pool PG.Connection -> Server HTTP.ProductRoute
productRoute pool = do
  let productConn = REPO.ProductConn pool
      f1 = SERVICE.findAll (REPO.findAll productConn)
      f2 = SERVICE.findById (REPO.findById productConn)
      f3 = SERVICE.create (REPO.create productConn)
      f4 = SERVICE.deleteById (REPO.deleteById productConn)
      f5 = SERVICE.update (REPO.update productConn)
  HTTP.routes f1 f2 f3 f4 f5

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
