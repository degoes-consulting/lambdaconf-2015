module Data.Monoid.Multiplicative where

import Control.Comonad
import Control.Extend
import Data.Monoid

-- | Monoid and semigroup for semirings under multiplication.
-- |
-- | ``` purescript
-- | Multiplicative x <> Multiplicative y == Multiplicative (x * y)
-- | mempty :: Multiplicative _ == Multiplicative one
-- | ```
newtype Multiplicative a = Multiplicative a

runMultiplicative :: forall a. Multiplicative a -> a
runMultiplicative (Multiplicative x) = x

instance eqMultiplicative :: (Eq a) => Eq (Multiplicative a) where
  (==) (Multiplicative x) (Multiplicative y) = x == y
  (/=) (Multiplicative x) (Multiplicative y) = x /= y

instance ordMultiplicative :: (Ord a) => Ord (Multiplicative a) where
  compare (Multiplicative x) (Multiplicative y) = compare x y

instance functorMultiplicative :: Functor Multiplicative where
  (<$>) f (Multiplicative x) = Multiplicative (f x)

instance applyMultiplicative :: Apply Multiplicative where
  (<*>) (Multiplicative f) (Multiplicative x) = Multiplicative (f x)

instance applicativeMultiplicative :: Applicative Multiplicative where
  pure = Multiplicative

instance bindMultiplicative :: Bind Multiplicative where
  (>>=) (Multiplicative x) f = f x

instance monadMultiplicative :: Monad Multiplicative

instance extendAdditive :: Extend Multiplicative where
  (<<=) f x = Multiplicative (f x)

instance comonadAdditive :: Comonad Multiplicative where
  extract = runMultiplicative

instance showMultiplicative :: (Show a) => Show (Multiplicative a) where
  show (Multiplicative a) = "Multiplicative (" ++ show a ++ ")"

instance semigroupMultiplicative :: (Semiring a) => Semigroup (Multiplicative a) where
  (<>) (Multiplicative a) (Multiplicative b) = Multiplicative (a * b)

instance monoidMultiplicative :: (Semiring a) => Monoid (Multiplicative a) where
  mempty = Multiplicative one
