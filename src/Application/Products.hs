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
  findById f id = f id
  findAll f = f
  create f product = create' f product

create' :: (Functor m, Monad m, MonadThrow m)
  => (DOMAIN.Product -> m Bool) -> DOMAIN.Product -> m ()
create' f' p = do
  result <- f' p
  if result then
    return ()
  else
    throwM $ DOMAIN.ProductException "I don't know changes"