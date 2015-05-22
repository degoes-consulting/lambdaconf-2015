-- | This module defines the `Alternative` type class and associated
-- | helper functions.

module Control.Alternative where

import Control.Alt
import Control.Lazy
import Control.Plus

-- | The `Alternative` type class has no members of its own; it just specifies
-- | that the type constructor has both `Applicative` and `Plus` instances.
-- |
-- | Types which have `Alternative` instances should also satisfy the following
-- | laws:
-- |
-- | - Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
-- | - Annihilation: `empty <*> f = empty`
class (Applicative f, Plus f) <= Alternative f

-- | Attempt a computation multiple times, requiring at least one success.
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
some :: forall f a. (Alternative f, Lazy1 f) => f a -> f [a]
some v = (:) <$> v <*> defer1 (\_ -> many v)

-- | Attempt a computation multiple times, returning as many successful results
-- | as possible (possibly zero).
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
many :: forall f a. (Alternative f, Lazy1 f) => f a -> f [a]
many v = some v <|> pure []

