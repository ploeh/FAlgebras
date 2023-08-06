module NonEmpty where

import Text.Read
import Data.List.NonEmpty (NonEmpty(..))
import Data.Functor.Classes
import Fix
import List
import Prelude hiding (head, tail)
import Control.Applicative (liftA2)

data NonEmptyF a c = NonEmptyF { head :: a, tail :: ListFix a }
                     deriving (Eq, Show, Read)

instance Functor (NonEmptyF a) where
  fmap _ (NonEmptyF x xs) = NonEmptyF x xs

instance Eq2 NonEmptyF where
  liftEq2 p _ (NonEmptyF x xs) (NonEmptyF y ys) = p x y && liftEq p xs ys

instance Eq a => Eq1 (NonEmptyF a) where
  liftEq = liftEq2 (==)

instance Show2 NonEmptyF where
  liftShowsPrec2 sa sas _ _ d (NonEmptyF x xs) =
    showsBinaryWith sa (liftShowsPrec sa sas) "NonEmptyF" d x xs

instance Show a => Show1 (NonEmptyF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Read2 NonEmptyF where
  liftReadPrec2 ra ras _ _ =
    readData (readBinaryWith ra (liftReadPrec ra ras) "NonEmptyF" NonEmptyF)

instance Read a => Read1 (NonEmptyF a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

newtype NonEmptyFix a =
  NonEmptyFix { unNonEmptyFix :: Fix (NonEmptyF a) } deriving (Eq, Show, Read)

createNonEmptyF :: a -> ListFix a -> NonEmptyFix a
createNonEmptyF x xs = NonEmptyFix $ Fix $ NonEmptyF x xs

-- This is, if you will, the general-purpose catamorphism for NonEmptyF.
nonEmptyF :: (a -> ListFix a -> c) -> NonEmptyFix a -> c
nonEmptyF f = cata alg . unNonEmptyFix
  where alg (NonEmptyF x xs) = f x xs

toListFix :: NonEmptyFix a -> ListFix a
toListFix = nonEmptyF consF

instance Semigroup (NonEmptyFix a) where
  xs <> ys =
    nonEmptyF (\x xs' -> createNonEmptyF x $ xs' <> toListFix ys) xs

instance Functor NonEmptyFix where
  fmap f = nonEmptyF (\x xs -> createNonEmptyF (f x) $ fmap f xs)

instance Applicative NonEmptyFix where
  pure x = createNonEmptyF x nilF
  liftA2 f xs ys =
    nonEmptyF
      (\x xs' ->
        nonEmptyF
          (\y ys' ->
            createNonEmptyF
              (f x y)
              (liftA2 f (consF x nilF) ys' <> liftA2 f xs' (consF y ys')))
          ys)
      xs

instance Monad NonEmptyFix where
  xs >>= f =
    nonEmptyF (\x xs' ->
      nonEmptyF
        (\y ys -> createNonEmptyF y $ ys <> (xs' >>= toListFix . f)) (f x)) xs

instance Foldable NonEmptyFix where
  foldr f seed = nonEmptyF (\x xs -> f x $ foldr f seed xs)

instance Traversable NonEmptyFix where
  traverse f = nonEmptyF (\x xs -> liftA2 createNonEmptyF (f x) (traverse f xs))

toNonEmpty :: NonEmptyFix a -> NonEmpty a
toNonEmpty = nonEmptyF (\x xs -> x :| toList xs)

fromNonEmpty :: NonEmpty a -> NonEmptyFix a
fromNonEmpty (x :| xs) = createNonEmptyF x $ fromList xs