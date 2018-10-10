module FullBinaryTree where

import Text.Read
import GHC.Read (expectP)
import Control.Applicative
import Data.Functor.Classes
import Fix

data FullBinaryTreeF a c = LeafF a | NodeF c a c deriving (Show, Eq, Read)

instance Functor (FullBinaryTreeF a) where
  fmap _     (LeafF x) = LeafF x
  fmap f (NodeF l x r) = NodeF (f l) x (f r)

instance Eq2 FullBinaryTreeF where
  liftEq2  p  _       (LeafF x)       (LeafF y) = p x y
  liftEq2 p1 p2 (NodeF l1 x r1) (NodeF l2 y r2) = p2 l1 l2 && p1 x y && p2 r1 r2
  liftEq2  _  _              _               _  = False

instance Eq a => Eq1 (FullBinaryTreeF a) where
  liftEq = liftEq2 (==)

instance Show2 FullBinaryTreeF where
  liftShowsPrec2 sa _  _ _ d     (LeafF x) = showsUnaryWith sa "LeafF" d x
  liftShowsPrec2 sa _ sb _ d (NodeF l v r) =
    showsTrinaryWith sb sa sb "NodeF" d l v r
    where
      showsTrinaryWith sp1 sp2 sp3 name i x y z =
        showParen (i > 10) $
        showString name .
        showChar ' ' .
        sp1 11 x .
        showChar ' ' .
        sp2 11 y .
        showChar ' ' .
        sp3 11 z

instance Show a => Show1 (FullBinaryTreeF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Read2 FullBinaryTreeF where
  liftReadPrec2 ra _ rb _ = readLeaf <|> readNode
    where
      readLeaf = readData (readUnaryWith ra "LeafF" LeafF)
      readNode = readData $ readTrinaryWith rb ra rb "NodeF" NodeF
      readTrinaryWith rp1 rp2 rp3 name cons = do
        expectP $ Ident name
        x <- step rp1
        y <- step rp2
        z <- step rp3
        return $ cons x y z

instance Read a => Read1 (FullBinaryTreeF a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

newtype FullBinaryTreeFix a =
  FullBinaryTreeFix { unFullBinaryTreeFix :: Fix (FullBinaryTreeF a) }
  deriving (Show, Eq, Read)

fbtLeafF :: a -> FullBinaryTreeFix a
fbtLeafF = FullBinaryTreeFix . Fix . LeafF

fbtNodeF :: FullBinaryTreeFix a -> a -> FullBinaryTreeFix a -> FullBinaryTreeFix a
fbtNodeF (FullBinaryTreeFix l) x (FullBinaryTreeFix r) =
  FullBinaryTreeFix $ Fix $ NodeF l x r

-- -- This is, if you will, the general-purpose catamorphism for FullBinaryTreeFix.
fullBinaryTreeF :: (c -> a -> c -> c) -> (a -> c) -> FullBinaryTreeFix a -> c
fullBinaryTreeF fn fl = cata alg . unFullBinaryTreeFix
  where alg     (LeafF x) = fl x
        alg (NodeF l x r) = fn l x r

instance Functor FullBinaryTreeFix where
  fmap f = fullBinaryTreeF (\l x r -> fbtNodeF l (f x) r) (fbtLeafF . f)

instance Foldable FullBinaryTreeFix where
  foldMap f = fullBinaryTreeF (\l x r -> l <> f x <> r) f

instance Traversable FullBinaryTreeFix where
  sequenceA = fullBinaryTreeF (liftA3 fbtNodeF) (fmap fbtLeafF)

countLeaves :: Num n => FullBinaryTreeFix a -> n
countLeaves = fullBinaryTreeF (\l _ r -> l + r) (const 1)

treeDepth :: (Ord n, Num n) => FullBinaryTreeFix a -> n
treeDepth = fullBinaryTreeF (\l _ r -> 1 + max l r) (const 0)