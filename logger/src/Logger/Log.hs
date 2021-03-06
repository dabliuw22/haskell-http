{-# LANGUAGE OverloadedStrings #-}

module Logger.Log (logger) where

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
    registerScribe,
  )
import Logger.Config.LogConfig (FromEnv (..), LogConfig (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.IO (stdout)

logger :: (LogEnv -> IO a) -> IO a
logger action = do
  envs <- fromEnv
  bracket (makeLogEnv envs) closeScribes action
  where
    makeLogEnv (LogConfig dir' filename' name env) = do
      let file = dir' <> "/" <> filename' -- dir' ++ "/" ++ filename'
      logEnv <-
        initLogEnv
          (fromString name)
          (fromString env)
      _ <- createDirectoryIfMissing True dir'
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      fileScribe <- mkFileScribe file (permitItem InfoS) V2
      newLogEnv <- registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv
      registerScribe "file" fileScribe defaultScribeSettings newLogEnv
