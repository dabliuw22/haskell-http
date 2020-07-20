{-# LANGUAGE DeriveDataTypeable #-}
module Domain.Products where

import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Data.Time (ZonedTime)

newtype ProductId = ProductId { id :: Text } deriving  (Show, Eq)

newtype ProductName = ProductName { name :: Text} deriving (Show, Eq)

newtype ProductStock = ProductStock { stock :: Double } deriving  (Show, Eq)

newtype ProductCreatedAt = ProductCreatedAt { createdAt :: ZonedTime } deriving Show

data Product = 
  Product {
    productId :: ProductId,
    productName :: ProductName,
    productStock :: ProductStock,
    productCreatedAt :: ProductCreatedAt
  } deriving Show

instance Eq Product where
  (==) p1 p2 = productId p1 == productId p2
  (/=) p1 p2 = productId p1 /= productId p2
  
newtype ProductException = ProductException String deriving (Show, Typeable)

instance Exception ProductException
