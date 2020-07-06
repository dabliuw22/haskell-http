{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Adapter.Http.Products where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant

type ProductApi = "products" :> Get '[JSON] [ProductDto]

data ProductDto = ProductDto { name :: String } deriving (Generic, Show)

instance ToJSON ProductDto

api :: Server ProductApi
api = return [ProductDto "Hello", ProductDto "World"]