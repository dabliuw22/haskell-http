{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Adapter.Http.GetProducts (GetProductRoute, routes) where

import Application.Products
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (ZonedTime)
import qualified Domain.Products as DOMAIN
import GHC.Generics (Generic)
import Servant
import Data.Maybe (fromJust)

type GetProductRoute =
  (
    Get '[JSON] [GetProductDto] :<|>
    Capture "id" Text :> Get '[JSON] GetProductDto
  ) -- or 
  {-
  "products" :> Get '[JSON] [GetProductDto] :<|>
  "products" :> Capture "id" Text :> Get '[JSON] GetProductDto
  -}
  
data GetProductDto = 
  GetProductDto {
    product_id:: Text,
    product_name :: Text,
    product_stock :: Double,
    product_created_at :: ZonedTime
  } deriving (Generic, Show)

instance ToJSON GetProductDto

routes :: IO [DOMAIN.Product]
  -> (Text -> IO (Maybe DOMAIN.Product))
  -> Server GetProductRoute
routes f1 f2 = allProducts f1 :<|> oneProduct f2 

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

to :: DOMAIN.Product -> GetProductDto
to p =
  GetProductDto {
    product_id = DOMAIN.id (DOMAIN.productId p),
    product_name = DOMAIN.name (DOMAIN.productName p),
    product_stock = DOMAIN.stock (DOMAIN.productStock p),
    product_created_at = DOMAIN.createdAt (DOMAIN.productCreatedAt p)
  }
