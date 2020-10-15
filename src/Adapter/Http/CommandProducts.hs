{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Adapter.Http.CommandProducts (CommandProductRoute, routes) where

import Application.Products ()
import Control.Applicative (empty)
import Control.Exception (try)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Time (ZonedTime, getZonedTime)
import qualified Data.UUID as UUID
import Data.UUID.V1 (nextUUID)
import qualified Domain.Products as DOMAIN
import GHC.Generics (Generic)
import Servant
  ( Capture,
    Delete,
    Handler,
    JSON,
    NoContent (..),
    PostCreated,
    Put,
    ReqBody,
    Server,
    ServerError (errBody),
    err400,
    throwError,
    type (:<|>) (..),
    type (:>),
  )

type CommandProductRoute =
  ( ReqBody '[JSON] CommandProductDto :> PostCreated '[JSON] NoContent
      :<|> Capture "id" Text :> Delete '[JSON] NoContent
      :<|> Capture "id" Text :> ReqBody '[JSON] CommandProductDto :> Put '[JSON] NoContent
  )

data CommandProductDto = CommandProductDto
  { product_name :: Text,
    product_stock :: Double
  }
  deriving (Show, Generic)

instance FromJSON CommandProductDto

routes ::
  (DOMAIN.Product -> IO ()) ->
  (Text -> IO ()) ->
  (Text -> Text -> Double -> IO ()) ->
  Server CommandProductRoute
routes f1 f2 f3 = createProduct f1 :<|> deleteProduct f2 :<|> updateProduct f3

createProduct ::
  (DOMAIN.Product -> IO ()) ->
  CommandProductDto ->
  Handler NoContent
createProduct f' dto' = do
  uuid <- liftIO nextUUID
  let uuid' = pack (UUID.toString (fromJust uuid))
  createdAt <- liftIO getZonedTime
  result <- liftIO $ try (f' (productfrom dto' uuid' createdAt))
  case result of
    Right _ -> return NoContent
    Left e ->
      case e of
        DOMAIN.ProductException _ ->
          throwError
            err400
              { errBody = "Error Create Product"
              }

deleteProduct :: (Text -> IO ()) -> Text -> Handler NoContent
deleteProduct f' id' = do
  result <- liftIO $ try (f' id')
  case result of
    Right _ -> return NoContent
    Left e ->
      case e of
        DOMAIN.ProductException _ ->
          throwError
            err400
              { errBody = "Error Delete Product"
              }

updateProduct ::
  (Text -> Text -> Double -> IO ()) ->
  Text ->
  CommandProductDto ->
  Handler NoContent
updateProduct f' id' dto' = do
  let name = product_name dto'
      stock = product_stock dto'
  result <- liftIO $ try (f' id' name stock)
  case result of
    Right _ -> return NoContent
    Left e ->
      case e of
        DOMAIN.ProductException _ ->
          throwError
            err400
              { errBody = "Error Update Product"
              }

productfrom :: CommandProductDto -> Text -> ZonedTime -> DOMAIN.Product
productfrom dto uuid createdAt =
  DOMAIN.Product
    { DOMAIN.productId = DOMAIN.ProductId uuid,
      DOMAIN.productName = DOMAIN.ProductName (product_name dto),
      DOMAIN.productStock = DOMAIN.ProductStock (product_stock dto),
      DOMAIN.productCreatedAt = DOMAIN.ProductCreatedAt createdAt
    }
