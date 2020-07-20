{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Adapter.Http.CommandProducts (CommandProductRoute, routes) where

import Application.Products
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import Data.Text (Text, pack)
import Data.Time (ZonedTime, getZonedTime)
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID
import qualified Domain.Products as DOMAIN
import GHC.Generics (Generic)
import Servant
import Data.Maybe (fromJust)

type CommandProductRoute = ReqBody '[JSON] CreateProductDto :> PostCreated '[JSON] ()

data CreateProductDto =
  CreateProductDto {
    name :: Text,
    stock :: Double
  } deriving (Generic, Show)
  
instance FromJSON CreateProductDto

routes ::  (DOMAIN.Product -> IO ())
  -> Server CommandProductRoute
routes = createProduct

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
                        
from :: CreateProductDto -> Text -> ZonedTime -> DOMAIN.Product
from dto uuid createdAt =
  DOMAIN.Product {
    DOMAIN.productId = DOMAIN.ProductId uuid,
    DOMAIN.productName = DOMAIN.ProductName (name dto),
    DOMAIN.productStock = DOMAIN.ProductStock (stock dto),
    DOMAIN.productCreatedAt = DOMAIN.ProductCreatedAt createdAt
  }
