{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Adapter.Http.Products (ProductRoute, routes) where

import Application.Products
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Text (Text)
import qualified Domain.Products as DOMAIN
import GHC.Generics
import Servant

type ProductRoute =
  "products" :> Get '[JSON] [ProductDto] :<|>
  "products" :> Capture "id" Text :> Get '[JSON] (Maybe ProductDto) :<|>
  "products" :> ReqBody '[JSON] ProductDto :> PostCreated '[JSON] ()

data ProductDto =
  ProductDto {
    product_id:: Text,
    product_name :: Text,
    product_stock :: Double
  } deriving (Generic, Show)

instance ToJSON ProductDto

instance FromJSON ProductDto

routes :: IO [DOMAIN.Product]
  -> (Text -> IO (Maybe DOMAIN.Product))
  -> (DOMAIN.Product -> IO ())
  -> Server ProductRoute
routes f1 f2 f3 = allProducts f1 :<|> oneProduct f2 :<|> createProduct f3

oneProduct :: (Text -> IO (Maybe DOMAIN.Product)) -> Text -> Handler (Maybe ProductDto)
oneProduct f' id' = do
  result <- liftIO $ f' id'
  case result of
        Just value -> return $ Just (toDto value)
        Nothing    -> throwError 
          err404 {
            errBody = "Not Found Product"
          }

allProducts :: IO [DOMAIN.Product] -> Handler [ProductDto]
allProducts f' = liftIO $ fmap (map toDto) f'

createProduct :: (DOMAIN.Product -> IO ()) -> ProductDto -> Handler ()
createProduct f' dto' = do 
  result <- liftIO $ try (f' (fromDto dto'))
  case result of
        Right v -> return v
        Left e  -> case e of
                        DOMAIN.ProductException s -> throwError 
                          err400 { 
                            errBody = "Error Create Product" 
                          }
                        _ -> throwError 
                          err500 {
                            errBody = "Error Create Product"
                          }

toDto :: DOMAIN.Product -> ProductDto
toDto p =
  ProductDto {
    product_id = DOMAIN.id (DOMAIN.productId p),
    product_name = DOMAIN.name (DOMAIN.productName p),
    product_stock = DOMAIN.stock (DOMAIN.productStock p)
  }

fromDto :: ProductDto -> DOMAIN.Product
fromDto dto =
  DOMAIN.Product {
    DOMAIN.productId = DOMAIN.ProductId (product_id dto),
    DOMAIN.productName = DOMAIN.ProductName (product_name dto),
    DOMAIN.productStock = DOMAIN.ProductStock (product_stock dto)
  }