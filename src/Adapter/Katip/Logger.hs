{-# LANGUAGE OverloadedStrings #-}
module Adapter.Katip.Logger (logger) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Katip
import System.IO (stdout)
import Data.Text (Text)

logger :: (LogEnv -> IO a) -> IO a
logger action = do
  bracket makeLogEnv closeScribes action
  where
    makeLogEnv = do
      logEnv <- initLogEnv "haskell-http" "env"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv