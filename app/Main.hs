module Main where

import Control.Exception (bracket)
import Server.Api (acquire, release, use)

main :: IO ()
main = bracket acquire release use
