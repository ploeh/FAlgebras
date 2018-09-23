module Either where

import Text.Read
import Control.Applicative
import Data.Functor.Classes
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..), bisequenceA)
import Fix

data EitherF l r c = LeftF l | RightF r deriving (Show, Eq, Read)

instance Functor (EitherF l r) where
  fmap _  (LeftF l) = LeftF l
  fmap _ (RightF r) = RightF r

instance (Eq l, Eq r) => Eq1 (EitherF l r) where
  liftEq _  (LeftF x)  (LeftF y) = x == y
  liftEq _ (RightF x) (RightF y) = x == y
  liftEq _         _          _  = False

instance (Show l, Show r) => Show1 (EitherF l r) where
  liftShowsPrec _ _ d  (LeftF l) = showsUnaryWith showsPrec  "LeftF" d l
  liftShowsPrec _ _ d (RightF r) = showsUnaryWith showsPrec "RightF" d r

instance (Read l, Read r) => Read1 (EitherF l r) where
  liftReadPrec _ _ = readLeft <|> readRight
    where
      readLeft  = readData (readUnaryWith readPrec  "LeftF"  LeftF)
      readRight = readData (readUnaryWith readPrec "RightF" RightF)

newtype EitherFix l r =
  EitherFix { unEitherFix :: Fix (EitherF l r) } deriving (Show, Eq, Read)

leftF :: l -> EitherFix l r
leftF = EitherFix . Fix . LeftF

rightF :: r -> EitherFix l r
rightF = EitherFix . Fix . RightF

-- This is, if you will, the general-purpose catamorphism for EitherFix.
eitherF :: (l -> c) -> (r -> c) -> EitherFix l r -> c
eitherF fl fr = cata alg . unEitherFix
  where alg  (LeftF l) = fl l
        alg (RightF r) = fr r

instance Bifunctor EitherFix where
  bimap f s = eitherF (leftF . f) (rightF . s)

instance Functor (EitherFix l) where
  fmap = second

instance Applicative (EitherFix l) where
  pure = rightF
  f <*> x = eitherF leftF (<$> x) f

instance Monad (EitherFix l) where
  x >>= f = eitherF leftF f x

instance Bifoldable EitherFix where
  bifoldMap = eitherF

instance Bitraversable EitherFix where
  bitraverse fl fr = eitherF (fmap leftF . fl) (fmap rightF . fr)

instance Foldable (EitherFix l) where
  foldMap = bifoldMap mempty

instance Traversable (EitherFix l) where
  sequenceA = bisequenceA . first pure

toEither :: EitherFix l r -> Either l r
toEither = eitherF Left Right

fromEither :: Either a b -> EitherFix a b
fromEither = EitherFix . ana coalg
  where coalg  (Left l) =  LeftF l
        coalg (Right r) = RightF r