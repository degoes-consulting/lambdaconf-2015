module Data.Monoid.All where

import Data.Monoid

-- | Boolean monoid and semigroup under conjunction.
-- |
-- | ``` purescript
-- | All x <> All y == All (x && y)
-- | mempty :: All == All true
-- | ```
newtype All = All Boolean

runAll :: All -> Boolean
runAll (All x) = x

instance eqAll :: Eq All where
  (==) (All x) (All y) = x == y
  (/=) (All x) (All y) = x /= y

instance ordAll :: Ord All where
  compare (All x) (All y) = compare x y

instance showAll :: Show All where
  show (All a) = "All (" ++ show a ++ ")"

instance semigroupAll :: Semigroup All where
  (<>) (All x) (All y) = All (x && y)

instance monoidAll :: Monoid All where
  mempty = All true
