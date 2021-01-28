module Domain.Repository (ProductRepository (..)) where

import Data.Text (Text)
import Domain.Products (Product)

class ProductRepository c where
  findById :: c -> Text -> IO (Maybe Product)
  findAll :: c -> IO [Product]
  create :: c -> Product -> IO Bool
  deleteById :: c -> Text -> IO Bool
  update :: c -> Text -> Text -> Double -> IO Bool

{-
{-# LANGUAGE MultiParamTypeClasses #-}
class Monad m => ProductRepository m c where
  findById :: c -> Text -> m (Maybe Product)
  findAll :: c -> m [Product]
  create :: c -> Product -> m Bool
  deleteById :: c -> Text -> m Bool
  update :: c -> Text -> Text -> Double -> m Bool
-}
