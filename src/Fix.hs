module Fix where

import Data.Functor.Classes
import Text.Read

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

eqFixF :: (f1 (Fix f1) -> f2 (Fix f2) -> b) -> Fix f1 -> Fix f2 -> b
eqFixF p (Fix f) (Fix g) = p f g

instance Eq1 f => Eq (Fix f) where
  (==) = eqFixF eq1

showFixF :: (Int -> f (Fix f) -> ShowS) -> Int -> Fix f -> ShowS
showFixF sp d (Fix f) = showsUnaryWith sp "Fix" d f

instance Show1 f => Show (Fix f) where
  showsPrec = showFixF showsPrec1

readFixF :: ReadPrec (f (Fix f)) -> ReadPrec (Fix f)
readFixF r = parens $ prec 10 $ do
  Ident "Fix" <- lexP
  Fix <$> step r

instance Read1 f => Read (Fix f) where
  readPrec = readFixF readPrec1