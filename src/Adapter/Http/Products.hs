{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Adapter.Http.Products (ProductRoute, routes) where

import Application.Products
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Text (Text, pack)
import Data.Time (ZonedTime, getZonedTime)
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID
import qualified Domain.Products as DOMAIN
import GHC.Generics (Generic)
import Servant
import Data.Maybe (fromJust)

type ProductRoute =
  "products" :> (
    Get '[JSON] [GetProductDto] :<|>
    Capture "id" Text :> Get '[JSON] GetProductDto :<|>
    ReqBody '[JSON] CreateProductDto :> PostCreated '[JSON] ()
  ) -- or 
  {-
  "products" :> Get '[JSON] [GetProductDto] :<|>
  "products" :> Capture "id" Text :> Get '[JSON] GetProductDto :<|>
  "products" :> ReqBody '[JSON] CreateProductDto :> PostCreated '[JSON] ()
  -}


data CreateProductDto =
  CreateProductDto {
    name :: Text,
    stock :: Double
  } deriving (Generic, Show)
  
data GetProductDto = 
  GetProductDto {
    product_id:: Text,
    product_name :: Text,
    product_stock :: Double,
    product_created_at :: ZonedTime
  } deriving (Generic, Show)

instance ToJSON GetProductDto

instance FromJSON CreateProductDto

routes :: IO [DOMAIN.Product]
  -> (Text -> IO (Maybe DOMAIN.Product))
  -> (DOMAIN.Product -> IO ())
  -> Server ProductRoute
routes f1 f2 f3 = allProducts f1 :<|> oneProduct f2 :<|> createProduct f3

oneProduct :: (Text -> IO (Maybe DOMAIN.Product)) -> Text -> Handler GetProductDto
oneProduct f' id' = do
  result <- liftIO $ f' id'
  case result of
        Just value -> return $ to value
        Nothing    -> throwError 
          err404 {
            errBody = "Not Found Product"
          }

allProducts :: IO [DOMAIN.Product] -> Handler [GetProductDto]
allProducts f' =  do 
  result <- liftIO $ try (fmap (map to) f')
  case result of
        Right v -> return v
        Left e  -> case e of
                        DOMAIN.ProductException s -> throwError 
                          err500 {
                            errBody = "Error Get All Products"
                          }

createProduct :: (DOMAIN.Product -> IO ()) -> CreateProductDto -> Handler ()
createProduct f' dto' = do
  uuid <- liftIO nextUUID
  let uuid' = pack (UUID.toString (fromJust uuid))
  createdAt <- liftIO getZonedTime
  result <- liftIO $ try (f' (from dto' uuid' createdAt))
  case result of
      Right v -> return v
      Left e  -> case e of
                      DOMAIN.ProductException s -> throwError 
                        err400 { 
                          errBody = "Error Create Product" 
                        }

to :: DOMAIN.Product -> GetProductDto
to p =
  GetProductDto {
    product_id = DOMAIN.id (DOMAIN.productId p),
    product_name = DOMAIN.name (DOMAIN.productName p),
    product_stock = DOMAIN.stock (DOMAIN.productStock p),
    product_created_at = DOMAIN.createdAt (DOMAIN.productCreatedAt p)
  }

from :: CreateProductDto -> Text -> ZonedTime -> DOMAIN.Product
from dto uuid createdAt =
  DOMAIN.Product {
    DOMAIN.productId = DOMAIN.ProductId uuid,
    DOMAIN.productName = DOMAIN.ProductName (name dto),
    DOMAIN.productStock = DOMAIN.ProductStock (stock dto),
    DOMAIN.productCreatedAt = DOMAIN.ProductCreatedAt createdAt
  }