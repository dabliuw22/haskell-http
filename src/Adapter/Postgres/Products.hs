{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Adapter.Postgres.Products (ProductRepository(..)) where

import Adapter.Postgres.Util.PostgresUtil
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField (toField)
import qualified Domain.Products as P

class (Monad m, Functor m) => ProductRepository m where
  findById :: Pool PG.Connection -> Text -> m (Maybe P.Product)
  findAll :: Pool PG.Connection -> m [P.Product]
  create :: Pool PG.Connection -> P.Product -> m ()

instance ProductRepository IO where
  findById pool id = do 
    result <- findById' pool id
    case result of
          Just value  -> return $ Just (to value)
          Nothing     -> return Nothing
  findAll pool = fmap (map to) (findAll' pool)
  create pool product = pure ()

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
findById' pool' id' = liftIO $ queryOne pool' sql [id' :: Text]
  where
    sql = "SELECT * FROM products WHERE id = ?"

findAll' :: MonadIO m => Pool PG.Connection 
  -> m [ProductRow]
findAll' pool' = liftIO $ queryListWithoutParams pool' sql
  where 
    sql = "SELECT * FROM products"
    
create' :: MonadIO m => Pool PG.Connection 
  -> P.Product -> m ()
create' pool' product = do
  let row = from product
  liftIO $ command pool' sql (_id row , _name row, _stock row)
  where
    sql = "INSERT INTO products(id, name, stock) VALUES (?, ?, ?)"

to :: ProductRow -> P.Product
to row = 
  P.Product {
    P.productId = P.ProductId (_id row),
    P.productName = P.ProductName (_name row),
    P.productStock = P.ProductStock (_stock row)
  }

from :: P.Product -> ProductRow
from p =
  ProductRow {
    _id = P.id (P.productId p),
    _name = P.name (P.productName p),
    _stock = P.stock (P.productStock p)
  }