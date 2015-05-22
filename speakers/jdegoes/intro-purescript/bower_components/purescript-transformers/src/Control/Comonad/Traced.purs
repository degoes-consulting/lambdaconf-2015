-- | This module defines the `Traced` comonad.

module Control.Comonad.Traced where

import Control.Comonad.Traced.Trans
import Data.Identity

-- | The `Traced` comonad is a synonym for the `TracedT` comonad transformer, applied
-- | to the `Identity` monad.
type Traced m = TracedT m Identity

-- | Unwrap a value in the `Traced` comonad.
runTraced :: forall m a. Traced m a -> m -> a
runTraced = runTracedT >>> runIdentity

-- | Create a value in context in the `Traced` comonad.
traced :: forall m a. (m -> a) -> Traced m a
traced = Identity >>> TracedT
