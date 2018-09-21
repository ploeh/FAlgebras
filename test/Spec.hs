module Main where

import Test.Framework (defaultMain)
import BooleanTests (booleanTests)
import NatTests (natTests)
import MaybeTests (maybeTests)

main :: IO ()
main = defaultMain (booleanTests ++ natTests ++ maybeTests)