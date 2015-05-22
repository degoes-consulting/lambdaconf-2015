module Data.Monoid.Additive where

import Control.Comonad
import Control.Extend
import Data.Monoid

-- | Monoid and semigroup for semirings under addition.
-- |
-- | ``` purescript
-- | Additive x <> Additive y == Additive (x + y)
-- | mempty :: Additive _ == Additive zero
-- | ```
newtype Additive a = Additive a

runAdditive :: forall a. Additive a -> a
runAdditive (Additive x) = x

instance eqAdditive :: (Eq a) => Eq (Additive a) where
  (==) (Additive x) (Additive y) = x == y
  (/=) (Additive x) (Additive y) = x /= y

instance ordAdditive :: (Ord a) => Ord (Additive a) where
  compare (Additive x) (Additive y) = compare x y

instance functorAdditive :: Functor Additive where
  (<$>) f (Additive x) = Additive (f x)

instance applyAdditive :: Apply Additive where
  (<*>) (Additive f) (Additive x) = Additive (f x)

instance applicativeAdditive :: Applicative Additive where
  pure = Additive

instance bindAdditive :: Bind Additive where
  (>>=) (Additive x) f = f x

instance monadAdditive :: Monad Additive

instance extendAdditive :: Extend Additive where
  (<<=) f x = Additive (f x)

instance comonadAdditive :: Comonad Additive where
  extract = runAdditive

instance showAdditive :: (Show a) => Show (Additive a) where
  show (Additive a) = "Additive (" ++ show a ++ ")"

instance semigroupAdditive :: (Semiring a) => Semigroup (Additive a) where
  (<>) (Additive a) (Additive b) = Additive (a + b)

instance monoidAdditive :: (Semiring a) => Monoid (Additive a) where
  mempty = Additive zero
