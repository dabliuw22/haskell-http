{-# LANGUAGE OverloadedStrings #-}
module Adapter.Katip.Logger (logger) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Katip
import System.IO (stdout)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)

logger :: (LogEnv -> IO a) -> IO a
logger action = do
  bracket makeLogEnv closeScribes action
  where
    logFilename = "haskell-http.log"
    makeLogEnv = do
      logEnv <- initLogEnv "haskell-http" "env"
      logsDirEnv <- lookupEnv "LOGS_DIR"
      let dir = fromMaybe "logs" logsDirEnv
          file = dir ++ "/" ++ logFilename
      dirCreated <- createDirectoryIfMissing True dir
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      fileScribe <- mkFileScribe file (permitItem InfoS) V2
      newLogEnv <- registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv
      registerScribe "file" fileScribe defaultScribeSettings newLogEnv