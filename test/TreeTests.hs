{-# LANGUAGE ScopedTypeVariables #-}
module TreeTests where

import Data.Word (Word8)
import qualified Data.Tree as T
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Monad
import Data.Foldable (fold)
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Test.QuickCheck
import Tree
import List

instance Arbitrary a => Arbitrary (T.Tree a) where
  arbitrary = sized genTree
    where
      genLeaf = fmap (\x -> T.Node x []) arbitrary
      genSubtree n = liftM2 T.Node arbitrary $ listOf $ genTree $ n `div` 2
      genTree 0 = genLeaf
      genTree n = oneof [genLeaf, genSubtree n]

{-# ANN module "HLint: ignore Use hierarchical imports" #-}
{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Monad law, left identity" #-}
{-# ANN module "HLint: ignore Monad law, right identity" #-}
{-# ANN module "HLint: ignore Use >=>" #-}
{-# ANN module "HLint: ignore Use traverse" #-}
treeTests =
  [
    testGroup "Tree properties" [
      testProperty "Isomorphic to Tree" $ do
        t :: T.Tree Integer <- resize 10 arbitrary
        return $ t === toTree (fromTree t)
      ,
      testProperty "Equals self" $ do
        t :: TreeFix String <- fromTree <$> resize 10 arbitrary
        return $ t === t
      ,
      testProperty "Equality" $ do
        x :: TreeFix Word8 <- fromTree <$> arbitrary
        y :: TreeFix Word8 <- fromTree <$> arbitrary

        let actual = x == y

        let expected = toTree x == toTree y
        return $ expected === actual
      ,
      testProperty "Show and read round-trips" $ do
        t :: TreeFix String <- fromTree <$> resize 10 arbitrary
        return $ t === read (show t)
      ,
      testProperty "First functor law" $ do
        t :: TreeFix String <- fromTree <$> resize 10 arbitrary
        return $ t === fmap id t
      ,
      testProperty "Second functor law" $ do
        t :: TreeFix Integer <- fromTree <$> resize 10 arbitrary
        f :: String -> Ordering <- arbitrary
        g :: Integer -> String <- arbitrary

        let left = fmap f $ fmap g t
        let right = fmap (f . g) t

        return $ left === right
      ,
      testProperty "Applicative identity law" $ do
        v :: TreeFix String <- fromTree <$> resize 10 arbitrary
        return $ v === (pure id <*> v)
      ,
      testProperty "Applicative composition law" $ do
        u :: TreeFix (String -> Ordering) <- fromTree <$> resize 5 arbitrary
        v :: TreeFix (Integer -> String) <- fromTree <$> resize 5 arbitrary
        w :: TreeFix Integer <- fromTree <$> resize 10 arbitrary

        let left = pure (.) <*> u <*> v <*> w
        let right = u <*> (v <*> w)

        return $ left === right
      ,
      testProperty "Applicative homomorphism law" $ do
        f :: String -> Double <- arbitrary
        x :: String <- arbitrary

        let left :: TreeFix Double = pure f <*> pure x
        let right = pure (f x)

        return $ left === right
      ,
      testProperty "Applicative interchange law" $ do
        u :: TreeFix (Integer -> String) <- fromTree <$> resize 10 arbitrary
        y :: Integer <- arbitrary

        let left = u <*> pure y
        let right = pure ($ y) <*> u

        return $ left === right
      ,
      testProperty "Applicative behaves like Data.Tree" $ do
        xt :: TreeFix Integer <- fromTree <$> resize 10 arbitrary
        ft :: TreeFix (Integer -> String) <- fromTree <$> resize 5 arbitrary

        let actual = ft <*> xt

        let expected = toTree ft <*> toTree xt
        return $ expected === toTree actual
      ,
      testProperty "Monad left identity law" $ do
        a :: String <- arbitrary
        k :: String -> TreeFix Integer <- (fromTree .) <$> resize 5 arbitrary

        let left = return a >>= k
        let right = k a

        return $ left === right
      ,
      testProperty "Monad right identity law" $ do
        m :: TreeFix Integer <- fromTree <$> resize 10 arbitrary

        let left = m >>= return
        let right = m

        return $ left === right
      ,
      testProperty "Monad associativity law" $ do
        m :: TreeFix String <- fromTree <$> resize 10 arbitrary
        k :: String -> TreeFix Integer <- (fromTree .) <$> resize 5 arbitrary
        h :: Integer -> TreeFix Ordering <- (fromTree .) <$> resize 5 arbitrary

        let left = m >>= (\x -> k x >>= h)
        let right = (m >>= k) >>= h

        return $ left === right
      ,
      testProperty "Monad behaves like Data.Tree" $ do
        t :: TreeFix Integer <- fromTree <$> resize 10 arbitrary
        f :: Integer -> TreeFix String <- (fromTree .) <$> resize 5 arbitrary

        let actual = t >>= f

        let expected = toTree t >>= (toTree . f)
        return $ expected === toTree actual
      ,
      testProperty "Foldable relates to Functor" $ do
        m :: TreeFix Integer <- fromTree <$> resize 10 arbitrary
        f :: Integer -> String <- arbitrary

        let left = foldMap f m
        let right = (fold . fmap f) m

        return $ left === right
      ,
      testProperty "Traversable naturality law" $ do
        l :: TreeFix (String, Integer) <- fromTree <$> resize 10 arbitrary
        let t = Just . snd -- Just an example of f a -> g a

        let left = (t . sequenceA) l
        let right = (sequenceA . fmap t) l

        return $ left === right
      ,
      testProperty "Traversable identity law" $ do
        l :: TreeFix String <- fromTree <$> resize 10 arbitrary

        let left = (sequenceA . fmap Identity) l
        let right = Identity l

        return $ left === right
      ,
      testProperty "Traversable composition law" $ do
        l :: TreeFix (Maybe (String, Integer)) <- fromTree <$> arbitrary

        let left = (sequenceA . fmap Compose) l
        let right = (Compose . fmap sequenceA . sequenceA) l

        return $ left === right
    ],
    testGroup "Tree examples" $ hUnitTestToTests $ TestList [
      "Count leaves" ~: do
        (expected, t) <-
          [
            (1, leafF 42),
            (1, nodeF 1 (consF (leafF 2) nilF)),
            (2, nodeF 1 (consF (leafF 2) $ consF (leafF 3) nilF)),
            (2, nodeF 1 (consF (leafF 2) $ consF (nodeF 3 (consF (leafF 4) nilF)) nilF)),
            (3, nodeF 1 (consF (leafF 2) $ consF (leafF 3) $ consF (leafF 4) nilF)),
            (1, nodeF 1 (consF (nodeF 2 (consF (leafF 3) nilF)) nilF)),
            (4, nodeF 42
                  (consF (nodeF 1337
                    (consF (leafF (-3)) nilF)) $
                   consF (nodeF 7
                    (consF (leafF (-99)) $
                     consF (leafF 100) $
                     consF (leafF 0) nilF))
                   nilF))
          ]
        let actual = countLeaves t
        return $ expected ~=? actual
      ,
      "Tree depth" ~: do
        (expected, t) <-
          [
            (0, leafF 42),
            (1, nodeF 1 (consF (leafF 2) nilF)),
            (1, nodeF 1 (consF (leafF 2) $ consF (leafF 3) nilF)),
            (2, nodeF 1 (consF (leafF 2) $ consF (nodeF 3 (consF (leafF 4) nilF)) nilF)),
            (1, nodeF 1 (consF (leafF 2) $ consF (leafF 3) $ consF (leafF 4) nilF)),
            (2, nodeF 1 (consF (nodeF 2 (consF (leafF 3) nilF)) nilF)),
            (2, nodeF 42
                  (consF (nodeF 1337
                    (consF (leafF (-3)) nilF)) $
                   consF (nodeF 7
                    (consF (leafF (-99)) $
                     consF (leafF 100) $
                     consF (leafF 0) nilF))
                   nilF))
          ]
        let actual = treeDepth t
        return $ expected ~=? actual
    ]
  ]