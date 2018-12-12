module Main where

import Test.Framework (defaultMain)
import BooleanTests (booleanTests)
import NatTests (natTests)
import MaybeTests (maybeTests)
import ListTests (listTests)
import EitherTests (eitherTests)
import TreeTests (treeTests)
import RoseTreeTests (roseTreeTests)

main :: IO ()
main =
  defaultMain (
    booleanTests ++
    natTests ++
    maybeTests ++
    listTests ++
    eitherTests ++
    treeTests ++
    roseTreeTests)
