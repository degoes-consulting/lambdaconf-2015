-- | This module defines the state monad transformer, `StateT`.

module Control.Monad.State.Trans where

import Control.Alt
import Control.Alternative
import Control.Plus
import Control.Monad.Trans
import Control.MonadPlus
import Control.Lazy
import Data.Tuple

-- | The state monad transformer.
-- |
-- | This monad transformer extends the base monad with the operations `get` 
-- | and `put` which can be used to model a single piece of mutable state.
-- |
-- | The `MonadState` type class describes the operations supported by this monad.
newtype StateT s m a = StateT (s -> m (Tuple a s))

-- | Run a computation in the `StateT` monad.
runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT s) = s

-- | Run a computation in the `StateT` monad, discarding the final state.
evalStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m a
evalStateT m s = fst <$> runStateT m s

-- | Run a computation in the `StateT` monad discarding the result.
execStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m s
execStateT m s = snd <$> runStateT m s

-- | Change the result type in a `StateT` monad action.
mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
mapStateT f m = StateT $ f <<< runStateT m

-- | Modify the final state in a `StateT` monad action.
withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
withStateT f s = StateT $ runStateT s <<< f

instance functorStateT :: (Monad m) => Functor (StateT s m) where
  (<$>) = liftM1

instance applyStateT :: (Monad m) => Apply (StateT s m) where
  (<*>) = ap

instance applicativeStateT :: (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return $ Tuple a s

instance altStateT :: (Monad m, Alt m) => Alt (StateT s m) where
  (<|>) x y = StateT $ \s -> runStateT x s <|> runStateT y s

instance plusStateT :: (Monad m, Plus m) => Plus (StateT s m) where
  empty = StateT $ \_ -> empty

instance alternativeStateT :: (Monad m, Alternative m) => Alternative (StateT s m)

instance bindStateT :: (Monad m) => Bind (StateT s m) where
  (>>=) (StateT x) f = StateT \s -> do
    Tuple v s' <- x s
    runStateT (f v) s'

instance monadStateT :: (Monad m) => Monad (StateT s m)

instance monadPlusStateT :: (MonadPlus m) => MonadPlus (StateT s m)

instance monadTransStateT :: MonadTrans (StateT s) where
  lift m = StateT \s -> do
    x <- m
    return $ Tuple x s

instance lazy1StateT :: Lazy1 (StateT s m) where
  defer1 f = StateT $ \s -> runStateT (f unit) s

liftCatchState :: forall s m e a. (m (Tuple a s) -> (e -> m (Tuple a s)) -> m (Tuple a s)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a
liftCatchState catch m h = StateT $ \s -> catch (runStateT m s) (\e -> runStateT (h e) s)

liftListenState :: forall s m a w. (Monad m) => (m (Tuple a s) -> m (Tuple (Tuple a s) w)) -> StateT s m a -> StateT s m (Tuple a w)
liftListenState listen m = StateT $ \s -> do
    Tuple (Tuple a s') w <- listen (runStateT m s)
    return $ Tuple (Tuple a w) s'

liftPassState :: forall s m a b w. (Monad m) => (m (Tuple (Tuple a s) b) -> m (Tuple a s)) -> StateT s m (Tuple a b) -> StateT s m a
liftPassState pass m = StateT $ \s -> pass $ do
    Tuple (Tuple a f) s' <- runStateT m s
    return $ Tuple (Tuple a s') f

liftCallCCState :: forall s m a b. ((((Tuple a s) -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
liftCallCCState callCC f = StateT $ \s -> callCC $ \c -> runStateT (f (\a -> StateT $ \_ -> c (Tuple a s))) s

liftCallCCState' :: forall s m a b. ((((Tuple a s) -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
liftCallCCState' callCC f = StateT $ \s -> callCC $ \c -> runStateT (f (\a -> StateT $ \s' -> c (Tuple a s'))) s
