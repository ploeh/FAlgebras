module BooleanTests where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Boolean

booleanTests =
  [
    testGroup "Boolean properties" $ hUnitTestToTests $ TestList [
      "Isomorphic to Bool" ~: do
        b <- [True, False]
        return $ b ~=? toBool (fromBool b)
      ,
      "Equals self" ~: do
        b <- [trueF, falseF]
        return $ b ~=? b
      ,
      "Equality" ~: do
        x <- [True, False]
        y <- [True, False]

        let actual = fromBool x == fromBool y

        let expected = x == y
        return $ expected ~=? actual
      ,
      "Show and read round-trips" ~: do
        b <- [trueF, falseF]
        return $ b ~=? read (show b)
      ,
      "And" ~: do
        x <- [True, False]
        y <- [True, False]
        return $ x && y ~=? toBool (andF (fromBool x) (fromBool y))
      ,
      "Or" ~: do
        x <- [True, False]
        y <- [True, False]
        return $ x || y ~=? toBool (orF (fromBool x) (fromBool y))
      ,
      "Not" ~: do
        b <- [True, False]
        return $ Prelude.not b ~=? toBool (notF $ fromBool b)
    ]
  ]