module RoseTree where

import Text.Read
import Control.Applicative
import Data.Functor.Classes
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (fold)
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..), bisequenceA)
import Data.Semigroup (Sum(..))
import Fix
import List
import Either (EitherFix, eitherF)

{-# ANN module "HLint: ignore Use hierarchical imports" #-}

data RoseTreeF a b c =
    NodeF { nodeValue :: a, nodes :: ListFix c }
  | LeafF { leafValue :: b }
  deriving (Show, Eq, Read)

instance Functor (RoseTreeF a b) where
  fmap f (NodeF x ns) = NodeF x $ fmap f ns
  fmap _    (LeafF x) = LeafF x

instance (Eq a, Eq b) => Eq1 (RoseTreeF a b) where
  liftEq p (NodeF x xs) (NodeF y ys) = x == y && liftEq p xs ys
  liftEq _    (LeafF x)    (LeafF y) = x == y
  liftEq _           _            _  = False

instance (Show a, Show b) => Show1 (RoseTreeF a b) where
  liftShowsPrec sp sl d (NodeF x ns) =
    showsBinaryWith showsPrec (liftShowsPrec sp sl) "NodeF" d x ns
  liftShowsPrec _ _ d (LeafF x) = showsUnaryWith showsPrec "LeafF" d x

instance (Read a, Read b) => Read1 (RoseTreeF a b) where
  liftReadPrec rp lp = readLeaf <|> readNode
    where
      readLeaf = readData (readUnaryWith readPrec "LeafF" LeafF)
      readNode = readData (readBinaryWith readPrec readListFix "NodeF" NodeF)
      readListFix = liftReadPrec rp lp

newtype RoseTreeFix a b =
  RoseTreeFix { unRoseTreeFix :: Fix (RoseTreeF a b) } deriving (Show, Eq, Read)

roseLeafF :: b -> RoseTreeFix a b
roseLeafF = RoseTreeFix . Fix . LeafF

roseNodeF :: a -> ListFix (RoseTreeFix a b) -> RoseTreeFix a b
roseNodeF x = RoseTreeFix . Fix . NodeF x . fmap unRoseTreeFix

-- This is, if you will, the general-purpose catamorphism for RoseTreeFix.
roseTreeF :: (a -> ListFix c -> c) -> (b -> c) -> RoseTreeFix a b -> c
roseTreeF fn fl = cata alg . unRoseTreeFix
  where alg (NodeF x ns) = fn x ns
        alg    (LeafF x) = fl x

instance Bifunctor RoseTreeFix where
  bimap f s = roseTreeF (roseNodeF . f) (roseLeafF . s)

instance Functor (RoseTreeFix a) where
  fmap = second

instance Bifoldable RoseTreeFix where
  bifoldMap f = roseTreeF (\x xs -> f x <> fold xs)

instance Foldable (RoseTreeFix a) where
  foldMap = bifoldMap mempty

instance Bitraversable RoseTreeFix where
  bitraverse f s =
    roseTreeF (\x xs -> roseNodeF <$> f x <*> sequenceA xs) (fmap roseLeafF . s)

instance Traversable (RoseTreeFix a) where
  sequenceA = bisequenceA . first pure

countLeaves :: (Bifoldable p, Num n) => p a b -> n
countLeaves = getSum . bifoldMap (const $ Sum 0) (const $ Sum 1)

fromTuple :: (a, b) -> RoseTreeFix a b
fromTuple (x, y) = roseNodeF x (consF (roseLeafF y) nilF)

fromEitherFix :: EitherFix a b -> RoseTreeFix a b
fromEitherFix = eitherF (`roseNodeF` nilF) roseLeafF

treeDepth :: RoseTreeFix a b -> Integer
treeDepth = roseTreeF (\_ xs -> 1 + maximum xs) (const 0)