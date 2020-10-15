--{-# OPTIONS_GHC -F -pgmF hspec-discover #-} -- for automated test discovery

import Application.ProductsSpec (spec)
import Control.Monad ((>>))
import Test.Hspec (hspec)

main :: IO ()
main =
  putStrLn "Init test.."
    >> hspec spec
    <* putStrLn "End test"
