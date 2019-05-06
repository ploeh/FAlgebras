module Fix where

import Data.Functor.Classes
import Text.Read

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

instance Eq1 f => Eq (Fix f) where
  (Fix f) == (Fix g) = eq1 f g

instance Show1 f => Show (Fix f) where
  showsPrec d (Fix f) = showsUnaryWith showsPrec1 "Fix" d f

instance Read1 f => Read (Fix f) where
  readPrec = parens $ prec 10 $ do
    Ident "Fix" <- lexP
    Fix <$> step (readS_to_Prec readsPrec1)