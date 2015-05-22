-- | This module defines the `Store` comonad.

module Control.Comonad.Store where

import Control.Comonad.Store.Trans
import Data.Identity
import Data.Tuple

-- | The `Store` comonad is a synonym for the `StoreT` comonad transformer, applied
-- | to the `Identity` monad.
type Store s a = StoreT s Identity a

-- | Unwrap a value in the `Store` comonad.
runStore :: forall s a. Store s a -> Tuple (s -> a) s
runStore s = swap (runIdentity <$> (swap $ runStoreT s))

-- | Create a value in context in the `Store` comonad.
store :: forall s a. (s -> a) -> s -> Store s a
store f x = StoreT $ Tuple (Identity f) x
