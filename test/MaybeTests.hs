{-# LANGUAGE ScopedTypeVariables #-}
module MaybeTests where

import Data.Word (Word8)
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Foldable
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Maybe

{-# ANN module "HLint: ignore Use hierarchical imports" #-}
{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Monad law, left identity" #-}
{-# ANN module "HLint: ignore Monad law, right identity" #-}
{-# ANN module "HLint: ignore Use >=>" #-}
{-# ANN module "HLint: ignore Use traverse" #-}
maybeTests =
  [
    testGroup "Maybe properties" [
      testProperty "Isomorphic to Maybe" $ do
        m :: Maybe Integer <- arbitrary
        return $ m === toMaybe (fromMaybe m)
      ,
      testProperty "Equals self" $ do
        m :: MaybeFix String <- fromMaybe <$> arbitrary
        return $ m === m
      ,
      testProperty "Equality" $ do
        x :: MaybeFix Word8 <- fromMaybe <$> arbitrary
        y :: MaybeFix Word8 <- fromMaybe <$> arbitrary

        let actual = x == y

        let expected = toMaybe x == toMaybe y
        return $ expected === actual
      ,
      testProperty "Show and read round-trips" $ do
        m :: MaybeFix String <- fromMaybe <$> arbitrary
        return $ m === read (show m)
      ,
      testProperty "First functor law" $ do
        m :: MaybeFix Integer <- fromMaybe <$> arbitrary
        return $ m === fmap id m
      ,
      testProperty "Second functor law" $ do
        m :: MaybeFix Integer <- fromMaybe <$> arbitrary
        f :: String -> Ordering <- arbitrary
        g :: Integer -> String <- arbitrary

        let left = fmap f $ fmap g m
        let right = fmap (f . g) m

        return $ left === right
      ,
      testProperty "Applicative identity law" $ do
        v :: MaybeFix String <- fromMaybe <$> arbitrary
        return $ v === (pure id <*> v)
      ,
      testProperty "Applicative composition law" $ do
        u :: MaybeFix (String -> Ordering) <- fromMaybe <$> arbitrary
        v :: MaybeFix (Integer -> String) <- fromMaybe <$> arbitrary
        w :: MaybeFix Integer <- fromMaybe <$> arbitrary

        let left = pure (.) <*> u <*> v <*> w
        let right = u <*> (v <*> w)

        return $ left === right
      ,
      testProperty "Applicative homomorphism law" $ do
        f :: String -> Double <- arbitrary
        x :: String <- arbitrary

        let left :: MaybeFix Double = pure f <*> pure x
        let right = pure (f x)

        return $ left === right
      ,
      testProperty "Applicative interchange law" $ do
        u :: MaybeFix (Integer -> String) <- fromMaybe <$> arbitrary
        y :: Integer <- arbitrary

        let left = u <*> pure y
        let right = pure ($ y) <*> u

        return $ left === right
      ,
      testProperty "Monad left identity law" $ do
        a :: String <- arbitrary
        k :: String -> MaybeFix Integer <- (fromMaybe .) <$> arbitrary

        let left = return a >>= k
        let right = k a

        return $ left === right
      ,
      testProperty "Monad right identity law" $ do
        m :: MaybeFix Integer <- fromMaybe <$> arbitrary

        let left = m >>= return
        let right = m

        return $ left === right
      ,
      testProperty "Monad associativity law" $ do
        m :: MaybeFix String <- fromMaybe <$> arbitrary
        k :: String -> MaybeFix Integer <- (fromMaybe .) <$> arbitrary
        h :: Integer -> MaybeFix Ordering <- (fromMaybe .) <$> arbitrary

        let left = m >>= (\x -> k x >>= h)
        let right = (m >>= k) >>= h

        return $ left === right
      ,
      testProperty "Foldable relates to Functor" $ do
        m :: MaybeFix Integer <- fromMaybe <$> arbitrary
        f :: Integer -> String <- arbitrary

        let left = foldMap f m
        let right = (fold . fmap f) m

        return $ left === right
      ,
      testProperty "Traversable naturality law" $ do
        m :: MaybeFix (String, Integer) <- fromMaybe <$> arbitrary
        let t = (: []) . snd -- Just an example of f a -> g a

        let left = (t . sequenceA) m
        let right = (sequenceA . fmap t) m

        return $ left === right
      ,
      testProperty "Traversable identity law" $ do
        m :: MaybeFix String <- fromMaybe <$> arbitrary

        let left = (sequenceA . fmap Identity) m
        let right = Identity m

        return $ left === right
      ,
      testProperty "Traversable composition law" $ do
        m :: MaybeFix [(String, Integer)] <- fromMaybe <$> arbitrary

        let left = (sequenceA . fmap Compose) m
        let right = (Compose . fmap sequenceA . sequenceA) m

        return $ left === right
    ]
  ]