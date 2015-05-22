-- | This module defines the `RWS` monad.

module Control.Monad.RWS where

import Control.Monad.RWS.Trans
import Data.Identity
import Data.Monoid
import Data.Tuple

-- | The `RWS` monad is a synonym for the `RWST` monad transformer, applied
-- | to the `Identity` monad.
type RWS r w s = RWST r w s Identity

-- | Create an action in the `RWS` monad from a function which uses the 
-- | global context and state explicitly.
rws :: forall r w s a. (r -> s -> See s a w) -> RWS r w s a
rws f = RWST \r s -> return $ f r s

-- | Run a computation in the `RWS` monad.
runRWS :: forall r w s a. RWS r w s a -> r -> s -> See s a w
runRWS m r s = runIdentity $ runRWST m r s

-- | Run a computation in the `RWS` monad, discarding the final state
evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
evalRWS m r s = runIdentity $ evalRWST m r s

-- | Run a computation in the `RWS` monad, discarding the result
execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
execRWS m r s = runIdentity $ execRWST m r s

-- | Change the types of the result and accumulator in a `RWS` action
mapRWS :: forall r w1 w2 s a1 a2. (See s a1 w1 -> See s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
mapRWS f = mapRWST (runIdentity >>> f >>> Identity)

-- | Change the type of the context in a `RWS` action
withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
withRWS = withRWST

-- | Get the context of a `RWS` action
ask :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m r
ask = RWST \r s -> pure $ mkSee s r mempty

-- | Locally change the context of a `RWS` action.
local :: forall  r w s m a. (r -> r) -> RWST r w s m a -> RWST r w s m a
local f m = RWST \r s -> runRWST m (f r) s

-- | Read a value which depends on the context in a `RWS` action.
reader :: forall r w s m a. (Applicative m, Monoid w) => (r -> a) -> RWST r w s m a
reader f = RWST \r s -> pure $ mkSee s (f r) mempty

-- | Write to the accumulator in a `RWS` action
writer :: forall r w s m a. (Applicative m) => Tuple a w -> RWST r w s m a
writer (Tuple a w) = RWST \_ s -> pure $ {state: s, result: a, log: w}

-- | Execute a `RWS` action, and return the changes to the accumulator along with the return value
listen :: forall r w s m a. (Monad m) => RWST r w s m a -> RWST r w s m (Tuple a w)
listen m = RWST \r s -> runRWST m r s >>= \{state = s', result = a, log = w} -> pure $ {state: s', result: Tuple a w, log: w}

-- | Execute a `RWS` action and modify the accumulator
pass :: forall r w s m a. (Monad m) => RWST r w s m (Tuple a (w -> w)) -> RWST r w s m a
pass m = RWST \r s -> runRWST m r s >>= \{result = Tuple a f, state = s', log = w} -> pure $ {state: s', result: a, log: f w}

-- | Append a value to the accumulator in a `RWS` action
tell :: forall r w s m. (Applicative m) => w -> RWST r w s m Unit
tell w = writer (Tuple unit w)

-- | Execute a `RWS` action, and return a value which depends on the accumulator along with the return value
listens :: forall r w s m a b. (Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (Tuple a b)
listens f m = RWST \r s -> runRWST m r s >>= \{state = s', result = a, log = w} -> pure $ {state: s', result: Tuple a (f w), log: w}

-- | Modify the accumulator in a `RWS` action
censor :: forall r w s m a. (Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
censor f m = RWST \r s -> runRWST m r s >>= \see -> pure $ see{log = f see.log}

-- | Get or modify the state in a `RWS` action
state :: forall r w s m a. (Applicative m, Monoid w) => (s -> Tuple a s) -> RWST r w s m a
state f = RWST \_ s -> case f s of Tuple a s' -> pure $ mkSee s' a mempty

-- | Get the state in a `RWS` action
get :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m s
get = state \s -> Tuple s s

-- | Get a value which depends on the state in a `RWS` action
gets :: forall r w s m a. (Applicative m, Monoid w) => (s -> a) -> RWST r w s m a
gets f = state \s -> Tuple (f s) s

-- | Set the state in a `RWS` action
put :: forall r w s m. (Applicative m, Monoid w) => s -> RWST r w s m Unit
put s = state \_ -> Tuple unit s

-- | Modify the state in a `RWS` action
modify :: forall r w s m. (Applicative m, Monoid w) => (s -> s) -> RWST r w s m Unit
modify f = state \s -> Tuple unit (f s)
