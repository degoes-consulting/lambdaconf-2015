module Data.Monoid.Endo where

import Data.Monoid

-- | Monoid of endomorphisms under composition.
-- |
-- | Composes of functions of type `a -> a`:
-- | ``` purescript
-- | Endo f <> Endo g == Endo (f <<< g)
-- | mempty :: Endo _ == Endo id
-- | ```
newtype Endo a = Endo (a -> a)

runEndo :: forall a. Endo a -> a -> a
runEndo (Endo f) = f

instance semigroupEndo :: Semigroup (Endo a) where
  (<>) (Endo f) (Endo g) = Endo (f <<< g)

instance monoidEndo :: Monoid (Endo a) where
  mempty = Endo id
