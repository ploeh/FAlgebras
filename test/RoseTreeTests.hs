{-# LANGUAGE ScopedTypeVariables #-}
module RoseTreeTests where

import Data.Word (Word8)
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Bifunctor (bimap)
import Data.Bifoldable (bifold, bifoldMap, bifoldl)
import Data.Bitraversable (bitraverse)
import Data.Foldable
import Control.Monad
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Test.QuickCheck
import RoseTree
import List

{-# ANN module "HLint: ignore Use hierarchical imports" #-}
{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Use traverse" #-}

instance (Arbitrary a, Arbitrary b) => Arbitrary (RoseTreeFix a b) where
  arbitrary = sized genTree
    where
      genLeaf = roseLeafF <$> arbitrary
      genSubtree n =
        liftM2 roseNodeF arbitrary $ fmap fromList $ listOf $ genTree $ n `div` 2
      genTree 0 = genLeaf
      genTree n = oneof [genLeaf, genSubtree n]

roseTreeTests =
  [
    testGroup "Rose tree properties" [
      testProperty "Equals self" $ do
        t :: RoseTreeFix Integer Integer <- resize 10 arbitrary
        return $ t === t
      ,
      testProperty "Show and read round-trips" $ do
        t :: RoseTreeFix Integer String <- resize 10 arbitrary
        return $ t === read (show t)
      ,
      testProperty "Bifunctor identity law" $ do
        t :: RoseTreeFix Integer String <- resize 10 arbitrary

        let left = bimap id id t
        let right = t

        return $ left === right
      ,
      testProperty "First functor law" $ do
        t :: RoseTreeFix Ordering Integer <- resize 10 arbitrary
        return $ t === fmap id t
      ,
      testProperty "Second functor law" $ do
        t :: RoseTreeFix String Integer <- resize 10 arbitrary
        f :: String -> Ordering <- arbitrary
        g :: Integer -> String <- arbitrary

        let left = fmap f $ fmap g t
        let right = fmap (f . g) t

        return $ left === right
      ,
      testProperty "Bifoldable relates to Bifunctor" $ do
        t :: RoseTreeFix Word8 String <- resize 10 arbitrary
        f :: Word8 -> [Integer] <- arbitrary
        g :: String -> [Integer] <- arbitrary

        let left = bifoldMap f g t
        let right = (bifold . bimap f g) t

        return $ left === right
      ,
      testProperty "Foldable relates to Functor" $ do
        t :: RoseTreeFix String Integer <- resize 10 arbitrary
        f :: Integer -> String <- arbitrary

        let left = foldMap f t
        let right = (fold . fmap f) t

        return $ left == right
      ,
      testProperty "Bitraversable naturality law" $ do
        tree :: RoseTreeFix Ordering Word8 <- resize 10 arbitrary
        f :: Ordering -> (String, Integer) <- arbitrary
        g :: Word8 -> (String, Integer) <- arbitrary
        let t = (: []) . snd -- Just an example of fa -> g a

        let left = bitraverse (t . f) (t . g) tree
        let right = (t . bitraverse f g) tree

        return $ left === right
      ,
      testProperty "Bitraversable identity law" $ do
        t :: RoseTreeFix Integer String <- resize 10 arbitrary

        let left = bitraverse Identity Identity t
        let right = Identity t

        return $ left === right
      ,
      testProperty "Bitraversable composition law" $ do
        t :: RoseTreeFix Integer String <- resize 3 arbitrary
        f1 :: Integer -> [Word8] <- resize 5 arbitrary
        f2 :: String -> [Ordering] <- resize 5 arbitrary
        g1 :: Word8 -> (String, Bool) <- arbitrary
        g2 :: Ordering -> (String, Integer) <- arbitrary

        let left = (Compose . fmap (bitraverse g1 g2) . bitraverse f1 f2) t
        let right = bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2) t

        return $ left === right
      ,
      testProperty "Traversable naturality law" $ do
        tree :: RoseTreeFix Bool (String, Integer) <- resize 10 arbitrary
        let t = (: []) . snd -- Just an example of f a -> g a

        let left = (t . sequenceA) tree
        let right = (sequenceA . fmap t) tree

        return $ left == right
      ,
      testProperty "Traversable identity law" $ do
        t :: RoseTreeFix Double String <- resize 10 arbitrary

        let left = (sequenceA . fmap Identity) t
        let right = Identity t

        return $ left == right
      ,
      testProperty "Traversable composition law" $ do
        t :: RoseTreeFix Bool [(String, Integer)] <- resize 4 arbitrary

        let left = (sequenceA . fmap Compose) t
        let right = (Compose . fmap sequenceA . sequenceA) t

        return $ left === right
    ],
    testGroup "Tree examples" $ hUnitTestToTests $ TestList [
      "Count leaves" ~: do
        (expected, t) <-
          [
            (1, roseLeafF "42"),
            (1, roseNodeF 1 (consF (roseLeafF "2") nilF)),
            (2, roseNodeF 1 (consF (roseLeafF "2") $ consF (roseLeafF "3") nilF)),
            (2, roseNodeF 1 (consF (roseLeafF "2") $ consF (roseNodeF 3 (consF (roseLeafF "4") nilF)) nilF)),
            (3, roseNodeF 1 (consF (roseLeafF "2") $ consF (roseLeafF "3") $ consF (roseLeafF "4") nilF)),
            (1, roseNodeF 1 (consF (roseNodeF 2 (consF (roseLeafF "3") nilF)) nilF)),
            (7, roseNodeF 42 (
                  consF (
                    roseNodeF 1337 (
                      consF (roseLeafF "foo") $
                      consF (roseLeafF "bar") nilF)) $
                  consF (
                    roseNodeF 2112 (
                      consF (
                        roseNodeF 90125 (
                          consF (roseLeafF "baz") $
                          consF (roseLeafF "qux") $
                          consF (roseLeafF "quux") nilF)) $
                      consF (roseLeafF "quuz") nilF)) $
                  consF (
                    roseLeafF "corge")
                  nilF))
          ]
        let actual = countLeaves t
        return $ expected ~=? actual
      ,
      "Tree depth" ~: do
        (expected, t) <-
          [
            (0, roseLeafF "42"),
            (1, roseNodeF 1 (consF (roseLeafF "2") nilF)),
            (1, roseNodeF 1 (consF (roseLeafF "2") $ consF (roseLeafF "3") nilF)),
            (2, roseNodeF 1 (consF (roseLeafF "2") $ consF (roseNodeF 3 (consF (roseLeafF "4") nilF)) nilF)),
            (1, roseNodeF 1 (consF (roseLeafF "2") $ consF (roseLeafF "3") $ consF (roseLeafF "4") nilF)),
            (2, roseNodeF 1 (consF (roseNodeF 2 (consF (roseLeafF "3") nilF)) nilF)),
            (3, roseNodeF 42 (
                  consF (
                    roseNodeF 1337 (
                      consF (roseLeafF "foo") $
                      consF (roseLeafF "bar") nilF)) $
                  consF (
                    roseNodeF 2112 (
                      consF (
                        roseNodeF 90125 (
                          consF (roseLeafF "baz") $
                          consF (roseLeafF "qux") $
                          consF (roseLeafF "quux") nilF)) $
                      consF (roseLeafF "quuz") nilF)) $
                  consF (
                    roseLeafF "corge")
                  nilF))
          ]
        let actual = treeDepth t
        return $ expected ~=? actual
    ]
  ]