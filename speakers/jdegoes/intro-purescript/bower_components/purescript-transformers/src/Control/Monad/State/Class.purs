-- | This module defines the `MonadState` type class and its instances.

module Control.Monad.State.Class where

import Control.Monad.Trans
import Control.Monad.State.Trans
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Reader.Trans
import Control.Monad.RWS.Trans
import Control.Monad.Writer.Trans
import Data.Monoid
import Data.Tuple

import qualified Control.Monad.RWS as RWS

-- | The `MonadState s` type class represents those monads which support a single piece of mutable
-- | state of type `s`.
-- |
-- | - `state f` updates the state using the function `f`.
-- |
-- | An implementation is provided for `StateT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - `do { get ; get } = get`
-- | - `do { put x ; put y } = put y`
-- | - `do { put x ; get } = put x $> x`
-- | - `do { s <- get ; put s } = pure unit`
-- |
class MonadState s m where
  state :: forall a. (s -> (Tuple a s)) -> m a

-- | Get the current state.
get :: forall m s. (Monad m, MonadState s m) => m s
get = state \s -> Tuple s s

-- | Get a value which depends on the current state.
gets :: forall s m a. (Monad m, MonadState s m) => (s -> a) -> m a
gets f = state \s -> Tuple (f s) s

-- | Set the state.
put :: forall m s. (Monad m, MonadState s m) => s -> m Unit
put s = state \_ -> Tuple unit s

-- | Modify the state by applying a function to the current state.
modify :: forall s m. (Monad m, MonadState s m) => (s -> s) -> m Unit
modify f = state \s -> Tuple unit (f s)

instance monadStateStateT :: (Monad m) => MonadState s (StateT s m) where
  state f = StateT $ return <<< f

instance monadStateStateT1 :: (Monad m, MonadState s m) => MonadState s (StateT s1 m) where
  state f = lift (state f)

instance monadStateErrorT :: (Monad m, MonadState s m) => MonadState s (ErrorT e m) where
  state f = lift (state f)

instance monadStateMaybeT :: (Monad m, MonadState s m) => MonadState s (MaybeT m) where
  state f = lift (state f)

instance monadStateReaderT :: (Monad m, MonadState s m) => MonadState s (ReaderT r m) where
  state f = lift (state f)

instance monadStateWriterT :: (Monad m, Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  state f = lift (state f)

instance monadStateRWST :: (Monad m, Monoid w) => MonadState s (RWST r w s m) where
  state = RWS.state
