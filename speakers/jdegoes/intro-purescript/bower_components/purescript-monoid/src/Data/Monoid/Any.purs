module Data.Monoid.Any where

import Data.Monoid

-- | Boolean monoid and semigroup under disjunction.
-- |
-- | ``` purescript
-- | Any x <> Any y == Any (x || y)
-- | mempty :: Any == Any false
-- | ```
newtype Any = Any Boolean

runAny :: Any -> Boolean
runAny (Any x) = x

instance eqAny :: Eq Any where
  (==) (Any x) (Any y) = x == y
  (/=) (Any x) (Any y) = x /= y

instance ordAny :: Ord Any where
  compare (Any x) (Any y) = compare x y

instance showAny :: Show Any where
  show (Any a) = "Any (" ++ show a ++ ")"

instance semigroupAny :: Semigroup Any where
  (<>) (Any x) (Any y) = Any (x || y)

instance monoidAny :: Monoid Any where
  mempty = Any false
