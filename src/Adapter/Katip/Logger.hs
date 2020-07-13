{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Adapter.Katip.Logger (logger) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
      logsDir <- lookupEnv "LOGS_DIR" 
        >>= \case
              Just p  -> return p
              Nothing -> return "logs"
      let file = logsDir ++ "/" ++ logFilename
      dirCreated <- createDirectoryIfMissing True logsDir
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      fileScribe <- mkFileScribe file (permitItem InfoS) V2
      newLogEnv <- registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv
      registerScribe "file" fileScribe defaultScribeSettings newLogEnv