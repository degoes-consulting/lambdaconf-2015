-- | This module defines the `Alt` type class.

module Control.Alt where

infixl 3 <|>

-- | The `Alt` type class identifies an associative operation on a type
-- | constructor.  It is similar to `Semigroup`, except that it applies to
-- | types of kind `* -> *`, like `Array` or `List`, rather than concrete types
-- | `String` or `Number`.
-- |
-- | `Alt` instances are required to satisfy the following laws:
-- |
-- | - Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
-- | - Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`
-- |
-- | For example, the `Array` (`[]`) type is an instance of `Alt`, where
-- | `(<|>)` is defined to be concatenation.
class (Functor f) <= Alt f where
  (<|>) :: forall a. f a -> f a -> f a
