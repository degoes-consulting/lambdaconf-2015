-- | This module defines the reader-writer-state monad transformer, `RWST`.

module Control.Monad.RWS.Trans where

import Control.Monad.Trans
import Data.Monoid
import Data.Tuple

type See s a w =
  { state  :: s
  , result :: a
  , log    :: w
  }

mkSee :: forall s a w. (Monoid w) => s -> a -> w -> See s a w
mkSee s a w = { state: s, result: a, log: w }

-- | The reader-writer-state monad transformer, which combines the operations
-- | of `ReaderT`, `WriterT` and `StateT` into a single monad transformer.
newtype RWST r w s m a = RWST (r -> s -> m (See s a w))

-- | Run a computation in the `RWST` monad.
runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (See s a w)
runRWST (RWST x) = x

-- | Run a computation in the `RWST` monad, discarding the final state.
evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)
evalRWST m r s = runRWST m r s >>= \see -> return (Tuple see.result see.log)

-- | Run a computation in the `RWST` monad, discarding the result.
execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)
execRWST m r s = runRWST m r s >>= \see -> return (Tuple see.state see.log)

-- | Change the result and accumulator types in a `RWST` monad action.
mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (See s a1 w1) -> m2 (See s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2
mapRWST f m = RWST \r s -> f $ runRWST m r s

-- | Change the context type in a `RWST` monad action.
withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a
withRWST f m = RWST \r s -> uncurry (runRWST m) (f r s)

instance functorRWST :: (Functor m) => Functor (RWST r w s m) where
  (<$>) f m = RWST \r s -> (\see -> see{result = f see.result}) <$> runRWST m r s

instance applyRWST :: (Bind m, Monoid w) => Apply (RWST r w s m) where
  (<*>) f m = RWST \r s ->
    runRWST f r s  >>= \{state = s',  result = f',  log = w'}  ->
    runRWST m r s' <#> \{state = s'', result = a'', log = w''} ->
    mkSee s'' (f' a'') (w' ++ w'')

instance bindRWST :: (Bind m, Monoid w) => Bind (RWST r w s m) where
  (>>=) m f = RWST \r s ->
    runRWST m     r s  >>= \{result = a, state = s', log = l} ->
    runRWST (f a) r s' <#> \see' ->
    see'{log = l ++ see'.log}

instance applicativeRWST :: (Monad m, Monoid w) => Applicative (RWST r w s m) where
  pure a = RWST \_ s -> pure $ mkSee s a mempty

instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)

instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s) where
  lift m = RWST \_ s -> m >>= \a -> return $ mkSee s a mempty
