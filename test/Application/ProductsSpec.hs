{-# LANGUAGE OverloadedStrings #-}
module Application.ProductsSpec where

import Application.Products
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Time (ZonedTime, getZonedTime)
import qualified Data.UUID as UUID
import Domain.Products
import Test.Hspec

spec :: Spec
spec = do
  describe "ProductService" $ do
    context "findById" $ do
      it "Should return a Product" $ do
        let id = "fake_id"
        expected <- f1' id
        result <- findById f1' id
        result `shouldBe` expected
      it "Should not return a Product" $ do
        result <- findById (\id -> return Nothing) "fake_id"
        result `shouldBe` Nothing
    context "findAll" $ do
      it "Should return a [Product]" $ do
        p <- f1' "fake_id"
        let expected = fromJust p
        result <- findAll (return [expected])
        result `shouldBe` [expected]
      it "Should return a []" $ do
        result <- findAll (return [])
        result `shouldBe` []
    context "create" $ do
      it "Should return ()" $ do
        p <- f1' "fake_id"
        let newProduct = fromJust p
        result <- create (f2' True) newProduct
        result `shouldBe` ()
      it "Should return ProductException" $ do
        p <- f1' "fake_id"
        let newProduct = fromJust p
        create (f2' False) newProduct 
          `shouldThrow` (== ProductException "I don't know changes")
        
f1' :: (MonadIO m) => Text -> m (Maybe Product)
f1' id = do
  createdAt <- liftIO getZonedTime
  return $ Just $ mkProduct id createdAt
  
f2' :: (MonadIO m) => Bool -> Product -> m Bool
f2' b p = return b
  
mkProduct :: Text -> ZonedTime -> Product
mkProduct id createdAt = Product {
  productId = ProductId id,
  productName = ProductName "product_name",
  productStock = ProductStock 100,
  productCreatedAt = ProductCreatedAt createdAt
}
  
status :: Maybe a -> Bool
status m = case m of
  Just v -> True
  _      -> False
