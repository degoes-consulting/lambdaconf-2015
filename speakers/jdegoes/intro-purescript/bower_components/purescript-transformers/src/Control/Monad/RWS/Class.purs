-- | This module defines the `MonadRWS` type class and its instances.

module Control.Monad.RWS.Class where

import Control.Monad.RWS.Trans
import Control.Monad.Error.Trans
import Control.Monad.Error
import Control.Monad.Maybe.Trans
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Data.Monoid

-- | `MonadRWS r w s` combines the operations and laws of the `MonadReader r`, 
-- | `MonadWriter w` and `MonadState s` type classes.
-- |
-- | An implementation is provided for `RWST`, and for other monad transformers
-- | defined in this library.
class (Monad m, Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) <= MonadRWS r w s m

instance monadRWSRWST :: (Monad m, Monoid w) => MonadRWS r w s (RWST r w s m)

instance monadRWSErrorT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (ErrorT e m)

instance monadRWSMaybeT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (MaybeT m)
