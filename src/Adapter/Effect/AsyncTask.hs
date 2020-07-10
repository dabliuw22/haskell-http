module Adapter.Effect.AsyncTask (AsyncTask(..)) where

import Control.Concurrent.Async (async, waitCatch)
import Control.Monad.Catch (MonadThrow, throwM)

class (Monad m, MonadThrow m) => AsyncTask m where
  run :: m a -> m a
  
instance AsyncTask IO where
  run action = do
    var <- async action
    result <- waitCatch var
    case result of
      Right v -> return v
      Left e -> throwM e