module Domain.Products where

import Control.Exception
import Data.Text (Text)
import Data.Typeable

newtype ProductId = ProductId { id :: Text } deriving Show

newtype ProductName = ProductName { name :: Text} deriving Show

newtype ProductStock = ProductStock { stock :: Double } deriving Show

data Product = 
  Product {
    productId :: ProductId,
    productName :: ProductName,
    productStock :: ProductStock
  } deriving Show
  
newtype ProductException = ProductException String deriving (Show, Typeable)

instance Exception ProductException
