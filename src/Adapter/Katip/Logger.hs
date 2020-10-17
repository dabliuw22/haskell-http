{-# LANGUAGE OverloadedStrings #-}

module Adapter.Katip.Logger (logger) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Katip
  ( ColorStrategy (ColorIfTerminal),
    LogEnv,
    Severity (InfoS),
    Verbosity (V2),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    mkFileScribe,
    mkHandleScribe,
    permitItem,
    registerScribe
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.IO (stdout)

logger :: (LogEnv -> IO a) -> IO a
logger action = do
  bracket makeLogEnv closeScribes action
  where
    logFilename = "haskell-http.log"
    makeLogEnv = do
      appName <- lookupEnv "APP_NAME"
      env <- lookupEnv "APP_ENV"
      logEnv <-
        initLogEnv
          (fromString $ fromMaybe "haskell-http" appName)
          (fromString $ fromMaybe "local" env)
      logsDirEnv <- lookupEnv "LOGS_DIR"
      let dir = fromMaybe "logs" logsDirEnv
          file = dir <> "/" <> logFilename -- dir ++ "/" ++ logFilename
      _ <- createDirectoryIfMissing True dir
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      fileScribe <- mkFileScribe file (permitItem InfoS) V2
      newLogEnv <- registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv
      registerScribe "file" fileScribe defaultScribeSettings newLogEnv
