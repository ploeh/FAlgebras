{-# LANGUAGE ScopedTypeVariables #-}
module NonEmptyTests where

import Data.Word (Word8)
import Data.Functor.Identity
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty(..), append)
import Data.Foldable (fold)
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import NonEmpty

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    return $ x :| xs

nonEmptyTests =
  [
    testGroup "NonEmpty properties" [
      testProperty "Isomorphic to NonEmpty" $ do
        xs :: NonEmpty Integer <- arbitrary
        return $ xs === toNonEmpty (fromNonEmpty xs)
      ,
      testProperty "Equals self" $ do
        l :: NonEmptyFix String <- fromNonEmpty <$> arbitrary
        return $ l === l
      ,
      testProperty "Equality" $ do
        x :: NonEmptyFix Word8 <- fromNonEmpty <$> arbitrary
        y :: NonEmptyFix Word8 <- fromNonEmpty <$> arbitrary

        let actual = x == y

        let expected = toNonEmpty x == toNonEmpty y
        return $ expected === actual
      ,
      testProperty "Show and read round-trips" $ do
        l :: NonEmptyFix String <- fromNonEmpty <$> arbitrary
        return $ l === read (show l)
      ,
      testProperty "Semigroup associativity" $ do
        x :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary
        y :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary
        z :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary

        let left = (x <> y) <> z
        let right = x <> (y <> z)

        return $ left === right
      ,
      testProperty "mappend behaves like NonEmpty append" $ do
        x :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary
        y :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary

        let actual = x <> y

        let expected = append (toNonEmpty x) (toNonEmpty y)
        return $ expected === toNonEmpty actual
      ,
      testProperty "First functor law" $ do
        l :: NonEmptyFix String <- fromNonEmpty <$> arbitrary
        return $ l === fmap id l
      ,
      testProperty "Second functor law" $ do
        l :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary
        f :: String -> Ordering <- arbitrary
        g :: Integer -> String <- arbitrary

        let left = fmap f $ fmap g l
        let right = fmap (f . g) l

        return $ left === right
      ,
      testProperty "Applicative identity" $ do
        l :: NonEmptyFix String <- fromNonEmpty <$> arbitrary
        return $ l === (pure id <*> l)
      ,
      testProperty "Applicative composition law" $ do
        u :: NonEmptyFix (String -> Ordering) <-
          fromNonEmpty <$> resize 10 arbitrary
        v :: NonEmptyFix (Integer -> String) <-
          fromNonEmpty <$> resize 10 arbitrary
        w :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary

        let left = pure (.) <*> u <*> v <*> w
        let right = u <*> (v <*> w)

        return $ left === right
      ,
      testProperty "Applicative homomorphism law" $ do
        f :: String -> Ordering <- arbitrary
        x :: String <- arbitrary

        let left :: NonEmptyFix Ordering = pure f <*> pure x
        let right = pure (f x)

        return $ left === right
      ,
      testProperty "Applicative interchange law" $ do
        u :: NonEmptyFix (String -> Ordering) <- fromNonEmpty <$> arbitrary
        y :: String <- arbitrary

        let left = u <*> pure y
        let right = pure ($ y) <*> u

        return $ left === right
      ,
      testProperty "Monad left identity" $ do
        a :: String <- arbitrary
        k :: String -> NonEmptyFix Integer <- (fromNonEmpty .) <$> arbitrary

        let left = return a >>= k
        let right = k a

        return $ left === right
      ,
      testProperty "Monad right identity" $ do
        m :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary

        let left = m >>= return
        let right = m

        return $ left === right
      ,
      testProperty "Monad associativity" $ do
        m :: NonEmptyFix String <- fromNonEmpty <$> arbitrary
        k :: String -> NonEmptyFix Integer <-
          (fromNonEmpty .) <$> resize 25 arbitrary
        h :: Integer -> NonEmptyFix Ordering <-
          (fromNonEmpty .) <$> resize 25 arbitrary

        let left = m >>= (\x -> k x >>= h)
        let right = (m >>= k) >>= h

        return $ left === right
      ,
      testProperty "Foldable relates to Functor" $ do
        m :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary
        f :: Integer -> String <- arbitrary

        let left = foldMap f m
        let right = (fold . fmap f) m

        return $ left === right
      ,
      testProperty "Traversable naturality law" $ do
        l :: NonEmptyFix (String, Integer) <- fromNonEmpty <$> arbitrary
        let t = (: []) . snd -- Just an example of f a -> g a

        let left = (t . sequenceA) l
        let right = (sequenceA . fmap t) l

        return $ left === right
      ,
      testProperty "Traversable identity law" $ do
        l :: NonEmptyFix Integer <- fromNonEmpty <$> arbitrary

        let left = (sequenceA . fmap Identity) l
        let right = Identity l

        return $ left === right
      ,
      testProperty "Traversable composition law" $ do
        l :: NonEmptyFix (Maybe (String, Integer)) <- fromNonEmpty <$> arbitrary

        let left = (sequenceA . fmap Compose) l
        let right = (Compose . fmap sequenceA . sequenceA) l

        return $ left === right
    ]
  ]