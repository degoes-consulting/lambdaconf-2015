-- | This module defines the `MonadPlus` type class.

module Control.MonadPlus where

import Control.Alternative
import Control.Plus

-- | The `MonadPlus` type class has no members of its own; it just specifies
-- | that the type has both `Monad` and `Alternative` instances.
-- |
-- | Types which have `MonadPlus` instances should also satisfy the following
-- | laws:
-- |
-- | - Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
-- | - Annihilation: `empty >>= f = empty`
class (Monad m, Alternative m) <= MonadPlus m

-- | Fail using `Plus` if a condition does not hold, or
-- | succeed using `Monad` if it does.
-- |
-- | For example:
-- |
-- | ```purescript
-- | import Data.Array
-- | 
-- | factors :: Number -> [Number]
-- | factors n = do
-- |   a <- 1 .. n
-- |   b <- 1 .. a
-- |   guard $ a * b == n
-- |   return a
-- | ```
guard :: forall m. (MonadPlus m) => Boolean -> m Unit
guard true = return unit
guard false = empty
