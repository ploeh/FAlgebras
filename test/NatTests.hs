{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
module NatTests where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Boolean
import Nat

natTests =
  [
    testGroup "Nat properties" [
      testProperty "Isomorphic to Num" $ do
        i <- choose @Integer (0, 1e4)
        return $ i === toNum (fromNum i)
      ,
      testProperty "Equals self" $ do
        n <- fromNum <$> choose @Integer (0, 1e4)
        return $ n === n
      ,
      testProperty "Equality" $ do
        x <- choose @Integer (0, 1e4)
        y <- choose @Integer (0, 1e4)

        let actual = fromNum x == fromNum y

        let expected = x == y
        return $ expected === actual
      ,
      testProperty "Show and read round-trips" $ do
        n <- fromNum <$> choose @Integer (0, 100)
        return $ n === read (show n)
      ,
      testProperty "Even" $ do
        i <- choose @Integer (0, 1e4)
        return $ Prelude.even i === toBool (evenF (fromNum i))
      ,
      testProperty "Odd" $ do
        i <- choose @Integer (0, 1e4)
        return $ Prelude.odd i === toBool (oddF (fromNum i))
      ,
      testProperty "Increment" $ do
        i <- choose @Integer (0, 1e4)
        return $ i + 1 === toNum (incF (fromNum i))
      ,
      testProperty "Addition" $ do
        x <- choose @Integer (0, 500)
        y <- choose (0, 500)
        return $ x + y === toNum (addF (fromNum x) (fromNum y))
      ,
      testProperty "Multiplication" $ do
        x <- choose @Integer (0, 25)
        y <- choose (0, 25)
        return $ x * y === toNum (multiplyF (fromNum x) (fromNum y))
    ]
  ]