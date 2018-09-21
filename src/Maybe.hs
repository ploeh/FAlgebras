module Maybe where

import Text.Read
import GHC.Read (expectP)
import Control.Applicative
import Data.Functor
import Data.Functor.Classes
import Fix

data MaybeF a c = NothingF | JustF a deriving (Show, Eq, Read)

instance Functor (MaybeF a) where
  fmap _  NothingF = NothingF
  fmap _ (JustF x) = JustF x

instance Eq2 MaybeF where
  liftEq2 _ _  NothingF  NothingF = True
  liftEq2 p _ (JustF x) (JustF y) = p x y
  liftEq2 _ _        _         _  = False

instance Eq a => Eq1 (MaybeF a) where
  liftEq = liftEq2 (==)

instance Show2 MaybeF where
  liftShowsPrec2  _ _ _ _ _  NothingF = showString "NothingF"
  liftShowsPrec2 sa _ _ _ d (JustF x) = showsUnaryWith sa "JustF" d x

instance Show a => Show1 (MaybeF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Read2 MaybeF where
  liftReadPrec2 ra _ _ _ = readNothing <|> readJust
    where
      readNothing = parens (expectP (Ident "NothingF") $> NothingF)
      readJust = readData (readUnaryWith ra "JustF" JustF)

instance Read a => Read1 (MaybeF a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

newtype MaybeFix a =
  MaybeFix { unMaybeFix :: Fix (MaybeF a) } deriving (Show, Eq, Read)

nothingF :: MaybeFix a
nothingF = MaybeFix $ Fix NothingF

justF :: a -> MaybeFix a
justF = MaybeFix . Fix . JustF

-- This is, if you will, the general-purpose catamorphism for MaybeFix.
maybeF :: c -> (a -> c) -> MaybeFix a -> c
maybeF n f = cata alg . unMaybeFix
  where alg  NothingF = n
        alg (JustF x) = f x

instance Functor MaybeFix where
  fmap f = maybeF nothingF (justF . f)

instance Applicative MaybeFix where
  pure = justF
  f <*> x = maybeF nothingF (<$> x) f

instance Monad MaybeFix where
  x >>= f = maybeF nothingF f x

instance Foldable MaybeFix where
  foldMap = maybeF mempty

instance Traversable MaybeFix where
  sequenceA = maybeF (pure nothingF) (justF <$>)

toMaybe :: MaybeFix a -> Maybe a
toMaybe = maybeF Nothing return

fromMaybe :: Maybe a -> MaybeFix a
fromMaybe = MaybeFix . ana coalg
  where coalg  Nothing = NothingF
        coalg (Just x) = JustF x