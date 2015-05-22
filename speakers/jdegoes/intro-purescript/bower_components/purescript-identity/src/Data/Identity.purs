module Data.Identity where

import Control.Comonad (Comonad, extract)
import Control.Extend (Extend, (<<=))
import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Traversable (Traversable, traverse, sequence)

newtype Identity a = Identity a

runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x

instance eqIdentity :: (Eq a) => Eq (Identity a) where
  (==) (Identity x) (Identity y) = x == y
  (/=) x y = not (x == y)

instance ordIdentity :: (Ord a) => Ord (Identity a) where
  compare (Identity x) (Identity y) = compare x y

instance showConst :: (Show a) => Show (Identity a) where
  show (Identity x) = "Identity (" ++ show x ++ ")"

instance functorIdentity :: Functor Identity where
  (<$>) f (Identity x) = Identity (f x)

instance applyIdentity :: Apply Identity where
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance applicativeIdentity :: Applicative Identity where
  pure = Identity

instance bindIdentity :: Bind Identity where
  (>>=) m f = f (runIdentity m)

instance monadIdentity :: Monad Identity

instance extendIdentity :: Extend Identity where
  (<<=) f m = Identity (f m)

instance comonadIdentity :: Comonad Identity where
  extract (Identity x) = x

instance foldableIdentity :: Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

instance traversableIdentity :: Traversable Identity where
  traverse f (Identity x) = Identity <$> f x
  sequence (Identity x) = Identity <$> x
