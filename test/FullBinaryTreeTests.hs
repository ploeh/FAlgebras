{-# LANGUAGE ScopedTypeVariables #-}
module FullBinaryTreeTests where

import Data.Word (Word8)
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Monad
import Data.Foldable (fold)
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Test.QuickCheck
import FullBinaryTree

instance Arbitrary a => Arbitrary (FullBinaryTreeFix a) where
  arbitrary = sized genTree
    where
      genLeaf = fmap fbtLeafF arbitrary
      genSubtree n =
        liftM3 fbtNodeF (genTree (n `div` 2)) arbitrary (genTree (n `div` 2))
      genTree 0 = genLeaf
      genTree n = oneof [genLeaf, genSubtree n]

{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Use traverse" #-}
fullBinaryTreeTests =
  [
    testGroup "Full binary tree properties" [
      testProperty "Equals self" $ do
        t :: FullBinaryTreeFix String <- arbitrary
        return $ t === t
      ,
      testProperty "Show and read round-trips" $ do
        t :: FullBinaryTreeFix Word <- arbitrary
        return $ t === read (show t)
      ,
      testProperty "First functor law" $ do
        t :: FullBinaryTreeFix String <- arbitrary
        return $ t === fmap id t
      ,
      testProperty "Second functor law" $ do
        t :: FullBinaryTreeFix Integer <- arbitrary
        f :: String -> Ordering <- arbitrary
        g :: Integer -> String <- arbitrary

        let left = fmap f $ fmap g t
        let right = fmap (f . g) t

        return $ left === right
      ,
      testProperty "Foldable relates to Functor" $ do
        m :: FullBinaryTreeFix Integer <- arbitrary
        f :: Integer -> String <- arbitrary

        let left = foldMap f m
        let right = (fold . fmap f) m

        return $ left === right
      ,
      testProperty "Traversable naturality law" $ do
        l :: FullBinaryTreeFix (String, Integer) <- arbitrary
        let t = Just . snd -- Just an example of f a -> g a

        let left = (t . sequenceA) l
        let right = (sequenceA . fmap t) l

        return $ left === right
      ,
      testProperty "Traversable identity law" $ do
        l :: FullBinaryTreeFix String <- arbitrary

        let left = (sequenceA . fmap Identity) l
        let right = Identity l

        return $ left === right
      ,
      testProperty "Traversable composition law" $ do
        l :: FullBinaryTreeFix (Maybe (String, Integer)) <- arbitrary

        let left = (sequenceA . fmap Compose) l
        let right = (Compose . fmap sequenceA . sequenceA) l

        return $ left === right
    ],
    testGroup "Full binary tree examples" $ hUnitTestToTests $ TestList [
      "Count leaves" ~: do
        (expected, fbt) <-
          [
            (1, fbtLeafF 42),
            (2, fbtNodeF (fbtLeafF 2) 1 (fbtLeafF 3)),
            (3, fbtNodeF (fbtNodeF (fbtLeafF 4) 3 (fbtLeafF 5)) 1 (fbtLeafF 2)),
            (3, fbtNodeF (fbtLeafF 2) 1 (fbtNodeF (fbtLeafF 4) 3 (fbtLeafF 5))),
            (4,
              fbtNodeF
                (fbtNodeF
                  (fbtLeafF 42)
                  1337
                  (fbtLeafF 2112))
                666
                (fbtNodeF
                  (fbtLeafF 90125)
                  5040
                  (fbtLeafF 1984))),
            (4,
              fbtNodeF
                (fbtNodeF
                  (fbtLeafF 42)
                  1337
                  (fbtNodeF
                    (fbtLeafF 2112)
                    5040
                    (fbtLeafF 1984)))
                2
                (fbtLeafF 90125))
          ]
        let actual = countLeaves fbt
        return $ expected ~=? actual
      ,
      "Tree depth" ~: do
        (expected, fbt) <-
          [
            (0, fbtLeafF 42),
            (1, fbtNodeF (fbtLeafF 2) 1 (fbtLeafF 3)),
            (2, fbtNodeF (fbtNodeF (fbtLeafF 4) 3 (fbtLeafF 5)) 1 (fbtLeafF 2)),
            (2, fbtNodeF (fbtLeafF 2) 1 (fbtNodeF (fbtLeafF 4) 3 (fbtLeafF 5))),
            (2,
              fbtNodeF
                (fbtNodeF
                  (fbtLeafF 42)
                  1337
                  (fbtLeafF 2112))
                666
                (fbtNodeF
                  (fbtLeafF 90125)
                  5040
                  (fbtLeafF 1984))),
            (3,
              fbtNodeF
                (fbtNodeF
                  (fbtLeafF 42)
                  1337
                  (fbtNodeF
                    (fbtLeafF 2112)
                    5040
                    (fbtLeafF 1984)))
                2
                (fbtLeafF 90125))
          ]
        let actual = treeDepth fbt
        return $ expected ~=? actual
    ]
  ]