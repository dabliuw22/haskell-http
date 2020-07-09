module Application.Products (ProductService(..)) where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Domain.Products as DOMAIN

class (Monad m, Functor m) => ProductService m where
  findById :: (Text -> m (Maybe DOMAIN.Product))
    -> Text -> m (Maybe DOMAIN.Product)
  findAll :: m [DOMAIN.Product] -> m [DOMAIN.Product]
  create :: (DOMAIN.Product -> m Bool) -> DOMAIN.Product -> m ()

instance ProductService IO where
  findById f id = findById' f id
  findAll f = findAll' f
  create f product = create' f product

findById' :: (Functor m, Monad m)
  => (Text -> m (Maybe DOMAIN.Product))
  -> Text -> m (Maybe DOMAIN.Product)
findById' f' = f'

findAll' :: (Functor m, Monad m)
  => m [DOMAIN.Product] -> m [DOMAIN.Product]
findAll' f' = f'

create' :: (Functor m, Monad m, MonadThrow m)
  => (DOMAIN.Product -> m Bool) -> DOMAIN.Product -> m ()
create' f' p = do
  result <- f' p
  if result then
    return ()
  else
    throwM $ DOMAIN.ProductException "I don't know changes"