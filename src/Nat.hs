module Nat where

import Text.Read
import GHC.Read (expectP)
import Control.Applicative
import Data.Functor
import Data.Functor.Classes
import Fix
import Boolean

data NatF a = ZeroF | SuccF a deriving (Show, Eq, Read)

instance Functor NatF where
  fmap _ ZeroF = ZeroF
  fmap f (SuccF x) = SuccF $ f x

instance Eq1 NatF where
  liftEq _    ZeroF     ZeroF  = True
  liftEq p (SuccF a) (SuccF b) = p a b
  liftEq _        _         _  = False

instance Show1 NatF where
  liftShowsPrec  _ _ _    ZeroF  = showString "ZeroF"
  liftShowsPrec sp _ d (SuccF x) = showsUnaryWith sp "SuccF" d x

instance Read1 NatF where
  liftReadPrec rp _ = readZero <|> readSucc
    where
      readZero = parens (expectP (Ident "ZeroF") $> ZeroF)
      readSucc = readData (readUnaryWith rp "SuccF" SuccF)

zeroF, oneF, twoF, threeF, fourF, fiveF, sixF, sevenF, eightF, nineF :: Fix NatF
zeroF  = Fix ZeroF
oneF   = Fix $ SuccF zeroF
twoF   = Fix $ SuccF oneF
threeF = Fix $ SuccF twoF
fourF  = Fix $ SuccF threeF
fiveF  = Fix $ SuccF fourF
sixF   = Fix $ SuccF fiveF
sevenF = Fix $ SuccF sixF
eightF = Fix $ SuccF sevenF
nineF  = Fix $ SuccF eightF

-- This is, if you will, the general-purpose catamorphism for Nat. I named it
-- `nat` with a lower-case 'n', because that seems to be the naming convention
-- for some other, well-known catamorphisms, such as `maybe` and `either`. In
-- this code base, however, I add an 'F' suffix (for 'Fix') in order to be able
-- to distinguish this function from the built-in function.
natF :: a -> (a -> a) -> Fix NatF -> a
natF z next = cata alg
  where alg ZeroF = z
        alg (SuccF predecessor) = next predecessor

evenF :: Fix NatF -> Fix BoolF
evenF = natF trueF notF

oddF :: Fix NatF -> Fix BoolF
oddF = notF . evenF

incF :: Fix NatF -> Fix NatF
incF = natF oneF $ Fix . SuccF

addF :: Fix NatF -> Fix NatF -> Fix NatF
addF x y = natF y incF x

multiplyF :: Fix NatF -> Fix NatF -> Fix NatF
multiplyF x y = natF zeroF (addF y) x

toNum :: Num a => Fix NatF -> a
toNum = natF 0 (+ 1)

fromNum :: (Eq a, Num a) => a -> Fix NatF
fromNum = ana coalg
  where coalg 0 = ZeroF
        coalg x = SuccF $ x - 1