# Module Documentation

## Module Control.Monad.State.Class


This module defines the `MonadState` type class and its instances.

#### `MonadState`

``` purescript
class MonadState s m where
  state :: forall a. (s -> Tuple a s) -> m a
```

The `MonadState s` type class represents those monads which support a single piece of mutable
state of type `s`.

- `state f` updates the state using the function `f`.

An implementation is provided for `StateT`, and for other monad transformers
defined in this library.

Laws:

- `do { get ; get } = get`
- `do { put x ; put y } = put y`
- `do { put x ; get } = put x $> x`
- `do { s <- get ; put s } = pure unit`


#### `get`

``` purescript
get :: forall m s. (Monad m, MonadState s m) => m s
```

Get the current state.

#### `gets`

``` purescript
gets :: forall s m a. (Monad m, MonadState s m) => (s -> a) -> m a
```

Get a value which depends on the current state.

#### `put`

``` purescript
put :: forall m s. (Monad m, MonadState s m) => s -> m Unit
```

Set the state.

#### `modify`

``` purescript
modify :: forall s m. (Monad m, MonadState s m) => (s -> s) -> m Unit
```

Modify the state by applying a function to the current state.

#### `monadStateStateT`

``` purescript
instance monadStateStateT :: (Monad m) => MonadState s (StateT s m)
```


#### `monadStateStateT1`

``` purescript
instance monadStateStateT1 :: (Monad m, MonadState s m) => MonadState s (StateT s1 m)
```


#### `monadStateErrorT`

``` purescript
instance monadStateErrorT :: (Monad m, MonadState s m) => MonadState s (ErrorT e m)
```


#### `monadStateMaybeT`

``` purescript
instance monadStateMaybeT :: (Monad m, MonadState s m) => MonadState s (MaybeT m)
```


#### `monadStateReaderT`

``` purescript
instance monadStateReaderT :: (Monad m, MonadState s m) => MonadState s (ReaderT r m)
```


#### `monadStateWriterT`

``` purescript
instance monadStateWriterT :: (Monad m, Monoid w, MonadState s m) => MonadState s (WriterT w m)
```


#### `monadStateRWST`

``` purescript
instance monadStateRWST :: (Monad m, Monoid w) => MonadState s (RWST r w s m)
```



## Module Control.Monad.State.Trans


This module defines the state monad transformer, `StateT`.

#### `StateT`

``` purescript
newtype StateT s m a
  = StateT (s -> m (Tuple a s))
```

The state monad transformer.

This monad transformer extends the base monad with the operations `get` 
and `put` which can be used to model a single piece of mutable state.

The `MonadState` type class describes the operations supported by this monad.

#### `runStateT`

``` purescript
runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
```

Run a computation in the `StateT` monad.

#### `evalStateT`

``` purescript
evalStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m a
```

Run a computation in the `StateT` monad, discarding the final state.

#### `execStateT`

``` purescript
execStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m s
```

Run a computation in the `StateT` monad discarding the result.

#### `mapStateT`

``` purescript
mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
```

Change the result type in a `StateT` monad action.

#### `withStateT`

``` purescript
withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
```

Modify the final state in a `StateT` monad action.

#### `functorStateT`

``` purescript
instance functorStateT :: (Monad m) => Functor (StateT s m)
```


#### `applyStateT`

``` purescript
instance applyStateT :: (Monad m) => Apply (StateT s m)
```


#### `applicativeStateT`

``` purescript
instance applicativeStateT :: (Monad m) => Applicative (StateT s m)
```


#### `altStateT`

``` purescript
instance altStateT :: (Monad m, Alt m) => Alt (StateT s m)
```


#### `plusStateT`

``` purescript
instance plusStateT :: (Monad m, Plus m) => Plus (StateT s m)
```


#### `alternativeStateT`

``` purescript
instance alternativeStateT :: (Monad m, Alternative m) => Alternative (StateT s m)
```


#### `bindStateT`

``` purescript
instance bindStateT :: (Monad m) => Bind (StateT s m)
```


#### `monadStateT`

``` purescript
instance monadStateT :: (Monad m) => Monad (StateT s m)
```


#### `monadPlusStateT`

``` purescript
instance monadPlusStateT :: (MonadPlus m) => MonadPlus (StateT s m)
```


#### `monadTransStateT`

``` purescript
instance monadTransStateT :: MonadTrans (StateT s)
```


#### `lazy1StateT`

``` purescript
instance lazy1StateT :: Lazy1 (StateT s m)
```


#### `liftCatchState`

``` purescript
liftCatchState :: forall s m e a. (m (Tuple a s) -> (e -> m (Tuple a s)) -> m (Tuple a s)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a
```


#### `liftListenState`

``` purescript
liftListenState :: forall s m a w. (Monad m) => (m (Tuple a s) -> m (Tuple (Tuple a s) w)) -> StateT s m a -> StateT s m (Tuple a w)
```


#### `liftPassState`

``` purescript
liftPassState :: forall s m a b w. (Monad m) => (m (Tuple (Tuple a s) b) -> m (Tuple a s)) -> StateT s m (Tuple a b) -> StateT s m a
```


#### `liftCallCCState`

``` purescript
liftCallCCState :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
```


#### `liftCallCCState'`

``` purescript
liftCallCCState' :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
```



## Module Control.Monad.State


This module defines the `State` monad.

#### `State`

``` purescript
type State s = StateT s Identity
```

The `State` monad is a synonym for the `StateT` monad transformer, applied
to the `Identity` monad.

#### `runState`

``` purescript
runState :: forall s a. State s a -> s -> Tuple a s
```

Run a computation in the `State` monad

#### `evalState`

``` purescript
evalState :: forall s a. State s a -> s -> a
```

Run a computation in the `State` monad, discarding the final state

#### `execState`

``` purescript
execState :: forall s a. State s a -> s -> s
```

Run a computation in the `State` monad, discarding the result

#### `mapState`

``` purescript
mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
```

Change the type of the result in a `State` action

#### `withState`

``` purescript
withState :: forall s a. (s -> s) -> State s a -> State s a
```

Modify the state in a `State` action