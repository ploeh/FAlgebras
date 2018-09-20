module Main where

import Test.Framework (defaultMain)
import BooleanTests (booleanTests)
import NatTests (natTests)

main :: IO ()
main = defaultMain (booleanTests ++ natTests)