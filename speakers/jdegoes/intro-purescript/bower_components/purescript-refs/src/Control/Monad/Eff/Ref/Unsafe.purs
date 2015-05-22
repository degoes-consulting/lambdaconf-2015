-- | Unsafe functions for working with mutable references.

module Control.Monad.Eff.Ref.Unsafe where

import Control.Monad.Eff
import Control.Monad.Eff.Ref

-- | This handler function unsafely removes the `Ref` effect from an
-- | effectful action.
-- |
-- | This function might be used when it is impossible to prove to the
-- | typechecker that a particular mutable reference does not escape
-- | its scope.
foreign import unsafeRunRef 
  "function unsafeRunRef(f) {\
  \  return f;\
  \}" :: forall eff a. Eff (ref :: Ref | eff) a -> Eff eff a
