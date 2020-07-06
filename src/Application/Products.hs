module Application.Products (ProductService(..)) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Domain.Products as DOMAIN

class (Monad m, Functor m) => ProductService m where
  findById :: (Text -> m (Maybe DOMAIN.Product)) 
    -> Text -> m (Maybe DOMAIN.Product)
  findAll :: m [DOMAIN.Product] -> m [DOMAIN.Product]
  create :: (DOMAIN.Product -> m ()) -> DOMAIN.Product -> m ()

instance ProductService IO where
  findById f id = findById' f id
  findAll f = findAll' f
  create f product = create' f product

findById' :: (Functor m, Monad m, MonadError e m, MonadThrow m)
  => (Text -> m (Maybe DOMAIN.Product)) 
  -> Text -> m (Maybe DOMAIN.Product)
findById' f' id' = f' id' `catchError` 
  \e -> throwM $ DOMAIN.ProductException "Error findById"

findAll' :: (Functor m, Monad m, MonadError e m, MonadThrow m)
  => m [DOMAIN.Product] -> m [DOMAIN.Product]
findAll' f' = f' `catchError` 
  \e -> throwM $ DOMAIN.ProductException "Error findAll"

create' :: (Functor m, Monad m, MonadError e m, MonadThrow m)
  => (DOMAIN.Product -> m ()) -> DOMAIN.Product -> m ()
create' f' product' = f' product' `catchError` 
  \e -> throwM $ DOMAIN.ProductException "Error create"