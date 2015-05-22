# Module Documentation

## Module Control.Monad.RWS.Class


This module defines the `MonadRWS` type class and its instances.

#### `MonadRWS`

``` purescript
class (Monad m, Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) <= MonadRWS r w s m where
```

`MonadRWS r w s` combines the operations and laws of the `MonadReader r`, 
`MonadWriter w` and `MonadState s` type classes.

An implementation is provided for `RWST`, and for other monad transformers
defined in this library.

#### `monadRWSRWST`

``` purescript
instance monadRWSRWST :: (Monad m, Monoid w) => MonadRWS r w s (RWST r w s m)
```


#### `monadRWSErrorT`

``` purescript
instance monadRWSErrorT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (ErrorT e m)
```


#### `monadRWSMaybeT`

``` purescript
instance monadRWSMaybeT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (MaybeT m)
```



## Module Control.Monad.RWS.Trans


This module defines the reader-writer-state monad transformer, `RWST`.

#### `See`

``` purescript
type See s a w = { log :: w, result :: a, state :: s }
```


#### `mkSee`

``` purescript
mkSee :: forall s a w. (Monoid w) => s -> a -> w -> See s a w
```


#### `RWST`

``` purescript
newtype RWST r w s m a
  = RWST (r -> s -> m (See s a w))
```

The reader-writer-state monad transformer, which combines the operations
of `ReaderT`, `WriterT` and `StateT` into a single monad transformer.

#### `runRWST`

``` purescript
runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (See s a w)
```

Run a computation in the `RWST` monad.

#### `evalRWST`

``` purescript
evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)
```

Run a computation in the `RWST` monad, discarding the final state.

#### `execRWST`

``` purescript
execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)
```

Run a computation in the `RWST` monad, discarding the result.

#### `mapRWST`

``` purescript
mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (See s a1 w1) -> m2 (See s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2
```

Change the result and accumulator types in a `RWST` monad action.

#### `withRWST`

``` purescript
withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a
```

Change the context type in a `RWST` monad action.

#### `functorRWST`

``` purescript
instance functorRWST :: (Functor m) => Functor (RWST r w s m)
```


#### `applyRWST`

``` purescript
instance applyRWST :: (Bind m, Monoid w) => Apply (RWST r w s m)
```


#### `bindRWST`

``` purescript
instance bindRWST :: (Bind m, Monoid w) => Bind (RWST r w s m)
```


#### `applicativeRWST`

``` purescript
instance applicativeRWST :: (Monad m, Monoid w) => Applicative (RWST r w s m)
```


#### `monadRWST`

``` purescript
instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)
```


#### `monadTransRWST`

``` purescript
instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s)
```



## Module Control.Monad.RWS


This module defines the `RWS` monad.

#### `RWS`

``` purescript
type RWS r w s = RWST r w s Identity
```

The `RWS` monad is a synonym for the `RWST` monad transformer, applied
to the `Identity` monad.

#### `rws`

``` purescript
rws :: forall r w s a. (r -> s -> See s a w) -> RWS r w s a
```

Create an action in the `RWS` monad from a function which uses the 
global context and state explicitly.

#### `runRWS`

``` purescript
runRWS :: forall r w s a. RWS r w s a -> r -> s -> See s a w
```

Run a computation in the `RWS` monad.

#### `evalRWS`

``` purescript
evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
```

Run a computation in the `RWS` monad, discarding the final state

#### `execRWS`

``` purescript
execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
```

Run a computation in the `RWS` monad, discarding the result

#### `mapRWS`

``` purescript
mapRWS :: forall r w1 w2 s a1 a2. (See s a1 w1 -> See s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
```

Change the types of the result and accumulator in a `RWS` action

#### `withRWS`

``` purescript
withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
```

Change the type of the context in a `RWS` action

#### `ask`

``` purescript
ask :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m r
```

Get the context of a `RWS` action

#### `local`

``` purescript
local :: forall r w s m a. (r -> r) -> RWST r w s m a -> RWST r w s m a
```

Locally change the context of a `RWS` action.

#### `reader`

``` purescript
reader :: forall r w s m a. (Applicative m, Monoid w) => (r -> a) -> RWST r w s m a
```

Read a value which depends on the context in a `RWS` action.

#### `writer`

``` purescript
writer :: forall r w s m a. (Applicative m) => Tuple a w -> RWST r w s m a
```

Write to the accumulator in a `RWS` action

#### `listen`

``` purescript
listen :: forall r w s m a. (Monad m) => RWST r w s m a -> RWST r w s m (Tuple a w)
```

Execute a `RWS` action, and return the changes to the accumulator along with the return value

#### `pass`

``` purescript
pass :: forall r w s m a. (Monad m) => RWST r w s m (Tuple a (w -> w)) -> RWST r w s m a
```

Execute a `RWS` action and modify the accumulator

#### `tell`

``` purescript
tell :: forall r w s m. (Applicative m) => w -> RWST r w s m Unit
```

Append a value to the accumulator in a `RWS` action

#### `listens`

``` purescript
listens :: forall r w s m a b. (Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (Tuple a b)
```

Execute a `RWS` action, and return a value which depends on the accumulator along with the return value

#### `censor`

``` purescript
censor :: forall r w s m a. (Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
```

Modify the accumulator in a `RWS` action

#### `state`

``` purescript
state :: forall r w s m a. (Applicative m, Monoid w) => (s -> Tuple a s) -> RWST r w s m a
```

Get or modify the state in a `RWS` action

#### `get`

``` purescript
get :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m s
```

Get the state in a `RWS` action

#### `gets`

``` purescript
gets :: forall r w s m a. (Applicative m, Monoid w) => (s -> a) -> RWST r w s m a
```

Get a value which depends on the state in a `RWS` action

#### `put`

``` purescript
put :: forall r w s m. (Applicative m, Monoid w) => s -> RWST r w s m Unit
```

Set the state in a `RWS` action

#### `modify`

``` purescript
modify :: forall r w s m. (Applicative m, Monoid w) => (s -> s) -> RWST r w s m Unit
```

Modify the state in a `RWS` action