module Boolean where

import Text.Read
import GHC.Read (expectP)
import Control.Applicative
import Data.Functor
import Data.Functor.Classes
import Fix

data BoolF a = TrueF | FalseF deriving (Show, Eq, Read)

instance Functor BoolF where
  fmap _  TrueF =  TrueF
  fmap _ FalseF = FalseF

instance Eq1 BoolF where
  liftEq _  TrueF  TrueF = True
  liftEq _ FalseF FalseF = True
  liftEq _      _      _ = False

instance Show1 BoolF where
  liftShowsPrec _ _ = showsPrec

instance Read1 BoolF where
  liftReadPrec _ _ = readTrue <|> readFalse
    where
      readTrue  = parens (expectP (Ident  "TrueF") $>  TrueF)
      readFalse = parens (expectP (Ident "FalseF") $> FalseF)

trueF, falseF :: Fix BoolF
trueF  = Fix  TrueF
falseF = Fix FalseF

-- This is, if you will, the general-purpose catamorphism for Boolean. I named
-- it `boolF` with a lower-case 'b', because that seems to be the naming
-- convention for some other, well-known catamorphisms, such as `maybe` and
-- `either`. In this code base, however, I add an 'F' suffix (for 'Fix') in
-- order to be able to distinguish this function from the built-in function.
-- Note that x corresponds to true, and y to false, which is consistent with how
-- the ordering is normally presented in lambda calculus, but is opposite of the
-- ordering of arguments for the `bool` function in Data.Bool.
boolF :: a -> a -> Fix BoolF -> a
boolF x y = cata alg
  where alg  TrueF = x
        alg FalseF = y

andF :: Fix BoolF -> Fix BoolF -> Fix BoolF
andF x y = boolF y falseF x

orF :: Fix BoolF -> Fix BoolF -> Fix BoolF
orF x y = boolF trueF y x

notF :: Fix BoolF -> Fix BoolF
notF = boolF falseF trueF

toBool :: Fix BoolF -> Bool
toBool = boolF True False

fromBool :: Bool -> Fix BoolF
fromBool = ana coalg
  where coalg  True = TrueF
        coalg False = FalseF