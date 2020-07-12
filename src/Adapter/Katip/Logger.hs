{-# LANGUAGE OverloadedStrings #-}
module Adapter.Katip.Logger (logger) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Katip
import System.IO (stdout)
import System.Directory (createDirectoryIfMissing)

logger :: (LogEnv -> IO a) -> IO a
logger action = do
  bracket makeLogEnv closeScribes action
  where
    makeLogEnv = do
      logEnv <- initLogEnv "haskell-http" "env"
      dirCreated <- createDirectoryIfMissing True "logs"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      fileScribe <- mkFileScribe "logs/log.log" (permitItem InfoS) V2
      newLogEnv <- registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv
      registerScribe "file" fileScribe defaultScribeSettings newLogEnv