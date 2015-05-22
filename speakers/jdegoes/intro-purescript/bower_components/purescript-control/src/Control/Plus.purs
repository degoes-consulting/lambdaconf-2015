-- | This module defines the `Plus` type class.

module Control.Plus where

import Control.Alt

-- | The `Plus` type class extends the `Alt` type class with a value that
-- | should be the left and right identity for `(<|>)`.
-- |
-- | It is similar to `Monoid`, except that it applies to types of
-- | kind `* -> *`, like `Array` or `List`, rather than concrete types like
-- | `String` or `Number`.
-- |
-- | `Plus` instances should satisfy the following laws:
-- |
-- | - Left identity: `empty <|> x == x`
-- | - Right identity: `x <|> empty == x`
-- | - Annihilation: `f <$> empty == empty`
class (Alt f) <= Plus f where
  empty :: forall a. f a
