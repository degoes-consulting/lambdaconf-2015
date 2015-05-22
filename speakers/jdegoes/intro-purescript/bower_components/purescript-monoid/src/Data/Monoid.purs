module Data.Monoid where

import Data.Array ()
import Data.Maybe

class (Semigroup m) <= Monoid m where
  mempty :: m

instance monoidString :: Monoid String where
  mempty = ""

instance monoidArray :: Monoid [a] where
  mempty = []

instance monoidUnit :: Monoid Unit where
  mempty = unit

instance monoidArr :: (Monoid b) => Monoid (a -> b) where
  mempty = const mempty

instance monoidMaybe :: (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing
