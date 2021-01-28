module Adapter.Effect.AsyncTask (AsyncTask (..)) where

import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Monad.Catch (MonadThrow, throwM)

class (Monad m, MonadThrow m) => AsyncTask m where
  run :: m a -> m (Async a)
  get :: Async a -> m a
  runBlock :: m a -> m a

instance AsyncTask IO where
  run = async
  get action = do
    result <- waitCatch action
    case result of
      Right v -> return v
      Left e -> throwM e
  runBlock action = run action >>= \var -> get var
