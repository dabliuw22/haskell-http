module Envs.Env (FromEnv (..)) where

class FromEnv a where
  fromEnv :: IO a
