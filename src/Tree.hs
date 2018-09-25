module Tree where

import qualified Data.Tree as T
import Text.Read
import Data.Functor.Classes
import Data.Foldable (fold)
import Fix
import List

{-# ANN module "HLint: ignore Use hierarchical imports" #-}

data TreeF a c = NodeF { nodeValue :: a, nodes :: ListFix c }
                 deriving (Show, Eq, Read)

instance Functor (TreeF a) where
  fmap f (NodeF x ns) = NodeF x $ fmap f ns

instance Eq a => Eq1 (TreeF a) where
  liftEq p (NodeF x xts) (NodeF y yts) = x == y && liftEq p xts yts

instance Show a => Show1 (TreeF a) where
  liftShowsPrec sp sl d (NodeF v ns) =
    showsBinaryWith showsPrec (liftShowsPrec sp sl) "NodeF" d v ns

instance Read a => Read1 (TreeF a) where
  liftReadPrec rp lp = readNode
    where
      readNode = readData (readBinaryWith readPrec readListFix "NodeF" NodeF)
      readListFix = liftReadPrec rp lp

newtype TreeFix a =
  TreeFix { unTreeFix :: Fix (TreeF a) } deriving (Show, Eq, Read)

leafF :: a -> TreeFix a
leafF x = TreeFix $ Fix $ NodeF x nilF

nodeF :: a -> ListFix (TreeFix a) -> TreeFix a
nodeF x = TreeFix . Fix . NodeF x . fmap unTreeFix

-- This is, if you will, the general-purpose catamorphism for TreeFix.
treeF :: (a -> ListFix c -> c) -> TreeFix a -> c
treeF f = cata alg . unTreeFix
  where alg (NodeF x ns) = f x ns

instance Functor TreeFix where
  fmap f = treeF (nodeF . f)

instance Applicative TreeFix where
  pure = leafF
  ft <*> xt = treeF (\f ts -> addNodes ts $ f <$> xt) ft
    where addNodes ns (TreeFix (Fix (NodeF x xs))) =
            TreeFix (Fix (NodeF x (xs <> (unTreeFix <$> ns))))

instance Monad TreeFix where
  t >>= f = treeF (\x ns -> addNodes ns $ f x) t
    where addNodes ns (TreeFix (Fix (NodeF x xs))) =
            TreeFix (Fix (NodeF x (xs <> (unTreeFix <$> ns))))

instance Foldable TreeFix where
  foldMap f = treeF (\x xs -> f x <> fold xs)

instance Traversable TreeFix where
  sequenceA = treeF (\x ns -> nodeF <$> x <*> sequenceA ns)

countLeaves :: Num n => TreeFix a -> n
countLeaves = treeF (\_ xs -> if null xs then 1 else sum xs)

treeDepth :: (Ord n, Num n) => TreeFix a -> n
treeDepth = treeF (\_ xs -> if null xs then 0 else 1 + maximum xs)

toTree :: TreeFix a -> T.Tree a
toTree = treeF (\x ns -> T.Node x $ toList ns)

fromTree :: T.Tree a -> TreeFix a
fromTree = TreeFix . ana coalg
  where coalg (T.Node x ns) = NodeF x (fromList ns)