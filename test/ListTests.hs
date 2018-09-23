{-# LANGUAGE ScopedTypeVariables #-}
module ListTests where

import Data.Word (Word8)
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Foldable (fold)
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import List

{-# ANN module "HLint: ignore Use hierarchical imports" #-}
{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Monad law, left identity" #-}
{-# ANN module "HLint: ignore Monad law, right identity" #-}
{-# ANN module "HLint: ignore Use >=>" #-}
{-# ANN module "HLint: ignore Use traverse" #-}
listTests =
  [
    testGroup "List properties" [
      testProperty "Isomorphic to []" $ do
        xs :: [Integer] <- arbitrary
        return $ xs === toList (fromList xs)
      ,
      testProperty "Equals self" $ do
        l :: ListFix String <- fromList <$> arbitrary
        return $ l === l
      ,
      testProperty "Equality" $ do
        x :: ListFix Word8 <- fromList <$> arbitrary
        y :: ListFix Word8 <- fromList <$> arbitrary

        let actual = x == y

        let expected = toList x == toList y
        return $ expected === actual
      ,
      testProperty "Show and read round-trips" $ do
        l :: ListFix String <- fromList <$> arbitrary
        return $ l === read (show l)
      ,
      testProperty "Semigroup associativity" $ do
        x :: ListFix Integer <- fromList <$> arbitrary
        y :: ListFix Integer <- fromList <$> arbitrary
        z :: ListFix Integer <- fromList <$> arbitrary

        let left = (x <> y) <> z
        let right = x <> (y <> z)

        return $ left === right
      ,
      testProperty "Monoid left identity" $ do
        l :: ListFix String <- fromList <$> arbitrary
        return $ l === mempty <> l
      ,
      testProperty "Monoid right identity" $ do
        l :: ListFix String <- fromList <$> arbitrary
        return $ l === l <> mempty
      ,
      testProperty "mappend behaves like list concatenation" $ do
        x :: ListFix Integer <- fromList <$> arbitrary
        y :: ListFix Integer <- fromList <$> arbitrary

        let actual = x <> y

        let expected = toList x ++ toList y
        return $ expected === toList actual
      ,
      testProperty "First functor law" $ do
        l :: ListFix String <- fromList <$> arbitrary
        return $ l === fmap id l
      ,
      testProperty "Second functor law" $ do
        l :: ListFix Integer <- fromList <$> arbitrary
        f :: String -> Ordering <- arbitrary
        g :: Integer -> String <- arbitrary

        let left = fmap f $ fmap g l
        let right = fmap (f . g) l

        return $ left === right
      ,
      testProperty "Applicative identity law" $ do
        v :: ListFix String <- fromList <$> arbitrary
        return $ v === (pure id <*> v)
      ,
      testProperty "Applicative composition law" $ do
        u :: ListFix (String -> Ordering) <- fromList <$> resize 10 arbitrary
        v :: ListFix (Integer -> String) <- fromList <$> resize 10 arbitrary
        w :: ListFix Integer <- fromList <$> arbitrary

        let left = pure (.) <*> u <*> v <*> w
        let right = u <*> (v <*> w)

        return $ left === right
      ,
      testProperty "Applicative homomorphism law" $ do
        f :: String -> Double <- arbitrary
        x :: String <- arbitrary

        let left :: ListFix Double = pure f <*> pure x
        let right = pure (f x)

        return $ left === right
      ,
      testProperty "Applicative interchange law" $ do
        u :: ListFix (Integer -> String) <- fromList <$> arbitrary
        y :: Integer <- arbitrary

        let left = u <*> pure y
        let right = pure ($ y) <*> u

        return $ left === right
      ,
      testProperty "Monad left identity law" $ do
        a :: String <- arbitrary
        k :: String -> ListFix Integer <- (fromList .) <$> arbitrary

        let left = return a >>= k
        let right = k a

        return $ left === right
      ,
      testProperty "Monad right identity law" $ do
        m :: ListFix Integer <- fromList <$> arbitrary

        let left = m >>= return
        let right = m

        return $ left === right
      ,
      testProperty "Monad associativity law" $ do
        m :: ListFix String <- fromList <$> arbitrary
        k :: String -> ListFix Integer <- (fromList .) <$> resize 25 arbitrary
        h :: Integer -> ListFix Ordering <- (fromList .) <$> resize 25 arbitrary

        let left = m >>= (\x -> k x >>= h)
        let right = (m >>= k) >>= h

        return $ left === right
      ,
      testProperty "Foldable relates to Functor" $ do
        m :: ListFix Integer <- fromList <$> arbitrary
        f :: Integer -> String <- arbitrary

        let left = foldMap f m
        let right = (fold . fmap f) m

        return $ left === right
      ,
      testProperty "Traversable naturality law" $ do
        l :: ListFix (String, Integer) <- fromList <$> arbitrary
        let t = (: []) . snd -- Just an example of f a -> g a

        let left = (t . sequenceA) l
        let right = (sequenceA . fmap t) l

        return $ left === right
      ,
      testProperty "Traversable identity law" $ do
        l :: ListFix String <- fromList <$> arbitrary

        let left = (sequenceA . fmap Identity) l
        let right = Identity l

        return $ left === right
      ,
      testProperty "Traversable composition law" $ do
        l :: ListFix (Maybe (String, Integer)) <- fromList <$> arbitrary

        let left = (sequenceA . fmap Compose) l
        let right = (Compose . fmap sequenceA . sequenceA) l

        return $ left === right
    ]
  ]