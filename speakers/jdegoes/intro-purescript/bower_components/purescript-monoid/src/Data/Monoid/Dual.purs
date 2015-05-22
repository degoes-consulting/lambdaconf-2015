module Data.Monoid.Dual where

import Control.Comonad
import Control.Extend
import Data.Monoid

-- | The dual of a monoid.
-- |
-- | ``` purescript
-- | Dual x <> Dual y == Dual (y <> x)
-- | mempty :: Dual _ == Dual mempty
-- | ```
newtype Dual a = Dual a

runDual :: forall a. Dual a -> a
runDual (Dual x) = x

instance eqDual :: (Eq a) => Eq (Dual a) where
  (==) (Dual x) (Dual y) = x == y
  (/=) (Dual x) (Dual y) = x /= y

instance ordDual :: (Ord a) => Ord (Dual a) where
  compare (Dual x) (Dual y) = compare x y

instance functorDual :: Functor Dual where
  (<$>) f (Dual x) = Dual (f x)

instance applyDual :: Apply Dual where
  (<*>) (Dual f) (Dual x) = Dual (f x)

instance applicativeDual :: Applicative Dual where
  pure = Dual

instance bindDual :: Bind Dual where
  (>>=) (Dual x) f = f x

instance monadDual :: Monad Dual

instance extendDual :: Extend Dual where
  (<<=) f x = Dual (f x)

instance comonadDual :: Comonad Dual where
  extract = runDual

instance showDual :: (Show a) => Show (Dual a) where
  show (Dual a) = "Dual (" ++ show a ++ ")"

instance semigroupDual :: (Semigroup a) => Semigroup (Dual a) where
  (<>) (Dual x) (Dual y) = Dual (y <> x)

instance monoidDual :: (Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty
