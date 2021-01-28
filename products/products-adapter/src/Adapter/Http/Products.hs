{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Adapter.Http.Products where

import Adapter.Http.CommandProducts as COMMAND
  ( CommandProductRoute,
    routes,
  )
import Adapter.Http.GetProducts as GET (GetProductRoute, routes)
import Data.Text (Text)
import Domain.Products (Product)
import Servant (Server, type (:<|>) (..), type (:>))

type ProductRoute =
  "products" :> (GET.GetProductRoute :<|> COMMAND.CommandProductRoute)

routes ::
  IO [Product] ->
  (Text -> IO (Maybe Product)) ->
  (Product -> IO ()) ->
  (Text -> IO ()) ->
  (Text -> Text -> Double -> IO ()) ->
  Server ProductRoute
routes f1 f2 f3 f4 f5 = GET.routes f1 f2 :<|> COMMAND.routes f3 f4 f5
