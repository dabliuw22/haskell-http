module Application.Products (ProductService (..)) where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class ()
import Data.Text (Text)
import qualified Domain.Products as DOMAIN

class (Monad m) => ProductService m where
  findById ::
    (Text -> m (Maybe DOMAIN.Product)) ->
    Text ->
    m (Maybe DOMAIN.Product)
  findAll :: m [DOMAIN.Product] -> m [DOMAIN.Product]
  create :: (DOMAIN.Product -> m Bool) -> DOMAIN.Product -> m ()
  deleteById :: (Text -> m Bool) -> Text -> m ()
  update ::
    (Text -> Text -> Double -> m Bool) ->
    Text ->
    Text ->
    Double ->
    m ()

instance ProductService IO where
  findById f id' = f id'
  findAll f = f
  create f product' = create' f product'
  deleteById = deleteById'
  update f id' name stock = update' f id' name stock

create' ::
  MonadThrow m =>
  (DOMAIN.Product -> m Bool) ->
  DOMAIN.Product ->
  m ()
create' f' p = do
  result <- f' p
  if result
    then return ()
    else throwM $ DOMAIN.ProductException "I don't know changes"

deleteById' ::
  MonadThrow m =>
  (Text -> m Bool) ->
  Text ->
  m ()
deleteById' f' id' = do
  result <- f' id'
  if result
    then return ()
    else throwM $ DOMAIN.ProductException "I don't know changes"

update' ::
  MonadThrow m =>
  (Text -> Text -> Double -> m Bool) ->
  Text ->
  Text ->
  Double ->
  m ()
update' f' id' name' stock' = do
  result <- f' id' name' stock'
  if result
    then return ()
    else throwM $ DOMAIN.ProductException "I don't know changes"
