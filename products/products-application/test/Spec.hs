import Application.ProductsSpec (spec)
import Control.Monad ((>>))
import Test.Hspec (hspec)

main :: IO ()
main =
  putStrLn "Init product-application test.."
    >> hspec spec
      <* putStrLn "End product-application test"
