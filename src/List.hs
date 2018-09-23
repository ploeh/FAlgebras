module List where

import Text.Read
import GHC.Read (expectP)
import Control.Applicative
import Data.Functor
import Data.Functor.Classes
import Fix

data ListF a c = NilF | ConsF a c deriving (Show, Eq, Read)

instance Functor (ListF a) where
  fmap _       NilF  = NilF
  fmap f (ConsF a c) = ConsF a $ f c

instance Eq2 ListF where
  liftEq2  _  _       NilF        NilF  = True
  liftEq2 p1 p2 (ConsF a c) (ConsF b d) = p1 a b && p2 c d
  liftEq2  _  _          _           _  = False

instance Eq a => Eq1 (ListF a) where
  liftEq = liftEq2 (==)

instance Show2 ListF where
  liftShowsPrec2  _ _  _ _ _       NilF  = showString "NilF"
  liftShowsPrec2 sa _ sb _ d (ConsF x c) = showsBinaryWith sa sb "ConsF" d x c

instance Show a => Show1 (ListF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Read2 ListF where
  liftReadPrec2 ra _ rb _ = readNil <|> readCons
    where
      readNil = parens (expectP (Ident "NilF") $> NilF)
      readCons = readData (readBinaryWith ra rb "ConsF" ConsF)

instance Read a => Read1 (ListF a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

newtype ListFix a =
  ListFix { unListFix :: Fix (ListF a) } deriving (Show, Eq, Read)

nilF :: ListFix a
nilF = ListFix $ Fix NilF

consF :: a -> ListFix a -> ListFix a
consF x = ListFix . Fix . ConsF x . unListFix

-- This is, if you will, the general-purpose catamorphism for ListFix.
-- Notice that it's identical to foldr.
listF :: (a -> c -> c) -> c -> ListFix a -> c
listF f n = cata alg . unListFix
  where alg       NilF  = n
        alg (ConsF h t) = f h t

instance Semigroup (ListFix a) where
  xs <> ys = listF consF ys xs

instance Monoid (ListFix a) where
  mempty = nilF

instance Functor ListFix where
  fmap f = listF (\h l -> consF (f h) l) nilF

instance Applicative ListFix where
  pure x = consF x nilF
  fs <*> xs = listF (\f acc -> (f <$> xs) <> acc) nilF fs

instance Monad ListFix where
  xs >>= f = listF (\x acc -> f x <> acc) nilF xs

instance Foldable ListFix where
  foldr = listF

instance Traversable ListFix where
  sequenceA = listF (\x acc -> consF <$> x <*> acc) (pure nilF)

toList :: ListFix a -> [a]
toList = listF (:) []

fromList :: [a] -> ListFix a
fromList = ListFix . ana coalg
  where coalg   []  = NilF
        coalg (h:t) = ConsF h t