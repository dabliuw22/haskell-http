{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Adapter.Postgres.Products (ProductRepository(..)) where

import qualified Adapter.Postgres.Util.PostgresUtil as UTIL
import Control.Exception (catch)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField (toField)
import qualified Domain.Products as DOMAIN

class (Monad m, Functor m) => ProductRepository m where
  findById :: Pool PG.Connection -> Text -> m (Maybe DOMAIN.Product)
  findAll :: Pool PG.Connection -> m [DOMAIN.Product]
  create :: Pool PG.Connection -> DOMAIN.Product -> m ()

instance ProductRepository IO where
  findById pool id = do 
    result <- findById' pool id
    case result of
          Just value  -> return $ Just (to value)
          Nothing     -> return Nothing
  findAll pool = fmap (map to) (findAll' pool)
  create pool product = create' pool product

data ProductRow =
  ProductRow {
    _id :: Text,
    _name :: Text,
    _stock :: Double
  }

instance FromRow ProductRow where
  fromRow = ProductRow <$> field <*> field <*> field

instance ToRow ProductRow where
  toRow p = [toField (_id p), toField (_name p), toField (_stock p)]

findById' :: MonadIO m => Pool PG.Connection 
  -> Text -> m (Maybe ProductRow)
findById' pool' id' = liftIO $ UTIL.queryOne pool' sql [id' :: Text]
  `catch` \e -> handlePgException e message
  where
    sql = "SELECT * FROM products WHERE id = ?"
    message = "Error findById"

findAll' :: MonadIO m => Pool PG.Connection 
  -> m [ProductRow]
findAll' pool' = liftIO $ UTIL.queryListWithoutParams pool' sql 
  `catch` \e -> handlePgException e message
  where 
    sql = "SELECT * FROM products"
    message = "Error findAll"
    
create' :: MonadIO m => Pool PG.Connection 
  -> DOMAIN.Product -> m ()
create' pool' product = do
  let row = from product
  liftIO $ UTIL.command pool' sql (_id row , _name row, _stock row)
    `catch` \e -> handlePgException e message
  where
    sql = "INSERT INTO products (id, name, stock) VALUES (?, ?, ?)"
    message = "Error create"

handlePgException :: (MonadIO m, MonadThrow m) => UTIL.PostgresException -> String -> m a
handlePgException e m = throwM $ DOMAIN.ProductException m

to :: ProductRow -> DOMAIN.Product
to row = 
  DOMAIN.Product {
    DOMAIN.productId = DOMAIN.ProductId (_id row),
    DOMAIN.productName = DOMAIN.ProductName (_name row),
    DOMAIN.productStock = DOMAIN.ProductStock (_stock row)
  }

from :: DOMAIN.Product -> ProductRow
from p =
  ProductRow {
    _id = DOMAIN.id (DOMAIN.productId p),
    _name = DOMAIN.name (DOMAIN.productName p),
    _stock = DOMAIN.stock (DOMAIN.productStock p)
  }