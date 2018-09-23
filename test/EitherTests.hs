{-# LANGUAGE ScopedTypeVariables #-}
module EitherTests where

import Data.Word (Word8)
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Bifunctor (bimap)
import Data.Bifoldable (bifold, bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.Foldable
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Either

{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Monad law, left identity" #-}
{-# ANN module "HLint: ignore Monad law, right identity" #-}
{-# ANN module "HLint: ignore Use >=>" #-}
{-# ANN module "HLint: ignore Use traverse" #-}
eitherTests =
  [
    testGroup "Either properties" [
      testProperty "Isomorphic to Either" $ do
        e :: Either String Integer <- arbitrary
        return $ e === toEither (fromEither e)
      ,
      testProperty "Equals self" $ do
        e :: EitherFix Integer String <- fromEither <$> arbitrary
        return $ e === e
      ,
      testProperty "Equality" $ do
        x :: EitherFix Word8 Ordering <- fromEither <$> arbitrary
        y :: EitherFix Word8 Ordering <- fromEither <$> arbitrary

        let actual = x == y

        let expected = toEither x == toEither y
        return $ expected === actual
      ,
      testProperty "Show and read round-trips" $ do
        e :: EitherFix Integer String <- fromEither <$> arbitrary
        return $ e === read (show e)
      ,
      testProperty "Bifunctor identity law" $ do
        e :: EitherFix Integer String <- fromEither <$> arbitrary

        let left = bimap id id e
        let right = e

        return $ left === right
      ,
      testProperty "First functor law" $ do
        e :: EitherFix Ordering Integer <- fromEither <$> arbitrary
        return $ e === fmap id e
      ,
      testProperty "Second functor law" $ do
        e :: EitherFix String Integer <- fromEither <$> arbitrary
        f :: String -> Ordering <- arbitrary
        g :: Integer -> String <- arbitrary

        let left = fmap f $ fmap g e
        let right = fmap (f . g) e

        return $ left === right
      ,
      testProperty "Applicative identity law" $ do
        v :: EitherFix Integer String <- fromEither <$> arbitrary
        return $ v == (pure id <*> v)
      ,
      testProperty "Applicative composition law" $ do
        u :: EitherFix Ordering (String -> Ordering) <- fromEither <$> arbitrary
        v :: EitherFix Ordering (Integer -> String) <- fromEither <$> arbitrary
        w :: EitherFix Ordering Integer <- fromEither <$> arbitrary

        let left = pure (.) <*> u <*> v <*> w
        let right = u <*> (v <*> w)

        return $ left === right
      ,
      testProperty "Applicative homomorphism law" $ do
        f :: String -> Double <- arbitrary
        x :: String <- arbitrary

        let left :: EitherFix Bool Double = pure f <*> pure x
        let right = pure (f x)

        return $ left === right
      ,
      testProperty "Applicative interchange law" $ do
        u :: EitherFix Double (Integer -> String) <- fromEither <$> arbitrary
        y :: Integer <- arbitrary

        let left = u <*> pure y
        let right = pure ($ y) <*> u

        return $ left === right
      ,
      testProperty "Monad left identity law" $ do
        a :: String <- arbitrary
        k :: String -> EitherFix Integer Integer <- (fromEither .) <$> arbitrary

        let left = return a >>= k
        let right = k a

        return $ left === right
      ,
      testProperty "Monad right identity law" $ do
        m :: EitherFix String Integer <- fromEither <$> arbitrary

        let left = m >>= return
        let right = m

        return $ left === right
      ,
      testProperty "Monad associativity law" $ do
        m :: EitherFix Bool String <- fromEither <$> arbitrary
        k :: String -> EitherFix Bool Integer <- (fromEither .) <$> arbitrary
        h :: Integer -> EitherFix Bool Ordering <- (fromEither .) <$> arbitrary

        let left = m >>= (\x -> k x >>= h)
        let right = (m >>= k) >>= h

        return $ left === right
      ,
      testProperty "Bifoldable relates to Bifunctor" $ do
        e :: EitherFix Word8 String <- fromEither <$> arbitrary
        f :: Word8 -> [Integer] <- arbitrary
        g :: String -> [Integer] <- arbitrary

        let left = bifoldMap f g e
        let right = (bifold . bimap f g) e

        return $ left === right
      ,
      testProperty "Foldable relates to Functor" $ do
        e :: EitherFix String Integer <- fromEither <$> arbitrary
        f :: Integer -> String <- arbitrary

        let left = foldMap f e
        let right = (fold . fmap f) e

        return $ left === right
      ,
      testProperty "Bitraversable naturality law" $ do
        e :: EitherFix Ordering Word8 <- fromEither <$> arbitrary
        f :: Ordering -> (String, Integer) <- arbitrary
        g :: Word8 -> (String, Integer) <- arbitrary
        let t = (: []) . snd -- Just an example of f a -> g a

        let left = bitraverse (t . f) (t . g) e
        let right = (t . bitraverse f g) e

        return $ left === right
      ,
      testProperty "Bitraversable identity law" $ do
        e :: EitherFix Integer String <- fromEither <$> arbitrary

        let left = bitraverse Identity Identity e
        let right = Identity e

        return $ left === right
      ,
      testProperty "Bitraversable composition law" $ do
        e :: EitherFix Integer String <- fromEither <$> arbitrary
        f1 :: Integer -> [Word8] <- arbitrary
        f2 :: String -> [Ordering] <- arbitrary
        g1 :: Word8 -> (String, Bool) <- arbitrary
        g2 :: Ordering -> (String, Integer) <- arbitrary

        let left = (Compose . fmap (bitraverse g1 g2) . bitraverse f1 f2) e
        let right = bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2) e

        return $ left === right
      ,
      testProperty "Traversable naturality law" $ do
        m :: EitherFix Bool (String, Integer) <- fromEither <$> arbitrary
        let t = (: []) . snd -- Just an example of f a -> g a

        let left = (t . sequenceA) m
        let right = (sequenceA . fmap t) m

        return $ left === right
      ,
      testProperty "Traversable identity law" $ do
        m :: EitherFix Double String <- fromEither <$> arbitrary

        let left = (sequenceA . fmap Identity) m
        let right = Identity m

        return $ left === right
      ,
      testProperty "Traversable composition law" $ do
        m :: EitherFix Bool [(String, Integer)] <- fromEither <$> arbitrary

        let left = (sequenceA . fmap Compose) m
        let right = (Compose . fmap sequenceA . sequenceA) m

        return $ left === right
    ]
  ]