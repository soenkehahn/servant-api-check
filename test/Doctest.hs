
module Main where

import           Test.DocTest

main :: IO ()
main = do
  doctest ["src"]
