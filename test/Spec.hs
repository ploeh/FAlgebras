module Main where

import Test.Framework (defaultMain)
import BooleanTests (booleanTests)

main :: IO ()
main = defaultMain booleanTests