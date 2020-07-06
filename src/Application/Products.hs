module Application.Products (ProductService(..)) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Domain.Products as P

class (Monad m, Functor m) => ProductService m where
  findById :: (Text -> m (Maybe P.Product)) -> Text -> m (Maybe P.Product)
  findAll :: m [P.Product] -> m [P.Product]
  create :: (P.Product -> m ()) -> P.Product -> m ()

instance ProductService IO where
  findById f id = findById' f id
  findAll f = findAll' f
  create f product = create' f product
  
findById' :: (Functor m, Monad m, MonadError e m, MonadThrow m) 
  => (Text -> m (Maybe P.Product)) -> Text -> m (Maybe P.Product)
findById' f' id' = f' id' `catchError` \e -> throwM $ P.ProductException "Error findById"

findAll' :: (Functor m, Monad m, MonadError e m, MonadThrow m) 
  => m [P.Product] -> m [P.Product]
findAll' f' = f' `catchError` \e -> throwM $ P.ProductException "Error findAll"

create' :: (Functor m, Monad m, MonadError e m, MonadThrow m) 
  => (P.Product -> m ()) -> P.Product -> m ()
create' f' product' = f' product' `catchError` \e -> throwM $ P.ProductException "Error create"