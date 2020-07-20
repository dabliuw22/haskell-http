{-# LANGUAGE DeriveDataTypeable #-}
module Domain.Products where

import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Data.Time (ZonedTime)

newtype ProductId = ProductId { id :: Text } deriving Show

newtype ProductName = ProductName { name :: Text} deriving Show

newtype ProductStock = ProductStock { stock :: Double } deriving Show

newtype ProductCreatedAt = ProductCreatedAt { createdAt :: ZonedTime } deriving Show

data Product = 
  Product {
    productId :: ProductId,
    productName :: ProductName,
    productStock :: ProductStock,
    productCreatedAt :: ProductCreatedAt
  } deriving Show
  
newtype ProductException = ProductException String deriving (Show, Typeable)

instance Exception ProductException
