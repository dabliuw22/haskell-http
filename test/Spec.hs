--{-# OPTIONS_GHC -F -pgmF hspec-discover #-} -- for automated test discovery

import Application.ProductsSpec
import Control.Monad ((>>))
import Test.Hspec

main :: IO ()
main = 
  putStrLn "Init test.." >>
    hspec spec <* 
      putStrLn "End test"
