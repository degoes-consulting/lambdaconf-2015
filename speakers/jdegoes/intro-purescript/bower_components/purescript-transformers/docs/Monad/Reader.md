# Module Documentation

## Module Control.Monad.Reader.Class


This module defines the `MonadReader` type class and its instances.

#### `MonadReader`

``` purescript
class MonadReader r m where
  ask :: m r
  local :: forall a. (r -> r) -> m a -> m a
```

The `MonadReader` type class represents those monads which support a global context via
`ask` and `local`.

- `ask` reads the current context.
- `local f x` changes the value of the local context during the execution of the action `x`.

An implementation is provided for `ReaderT`, and for other monad transformers
defined in this library.

Laws:

- `do { ask ; ask } = ask`
- `local f ask = f <$> ask`
- `local _ (pure a) = pure a`
- `local f (do { a <- x ; y }) = do { a <- local f x ; local f y }` 

#### `reader`

``` purescript
reader :: forall r m a. (Monad m, MonadReader r m) => (r -> a) -> m a
```

Read a value which depends on the global context in any `MonadReader`.

#### `monadReaderFun`

``` purescript
instance monadReaderFun :: MonadReader r (Prim.Function r)
```


#### `monadReaderReaderT`

``` purescript
instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m)
```


#### `monadReaderErrorT`

``` purescript
instance monadReaderErrorT :: (Monad m, MonadReader r m) => MonadReader r (ErrorT e m)
```


#### `monadReaderMaybeT`

``` purescript
instance monadReaderMaybeT :: (Monad m, MonadReader r m) => MonadReader r (MaybeT m)
```


#### `monadReaderWriterT`

``` purescript
instance monadReaderWriterT :: (Monad m, Monoid w, MonadReader r m) => MonadReader r (WriterT w m)
```


#### `monadReaderStateT`

``` purescript
instance monadReaderStateT :: (Monad m, MonadReader r m) => MonadReader r (StateT s m)
```


#### `monadReaderRWST`

``` purescript
instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m)
```



## Module Control.Monad.Reader.Trans


This module defines the reader monad transformer, `ReaderT`.

#### `ReaderT`

``` purescript
newtype ReaderT r m a
  = ReaderT (r -> m a)
```

The reader monad transformer.

This monad transformer extends the base monad transformer with a _global context_ of
type `r`.

The `MonadReader` type class describes the operations supported by this monad.

#### `runReaderT`

``` purescript
runReaderT :: forall r m a. ReaderT r m a -> r -> m a
```

Run a computation in the `ReaderT` monad.

#### `withReaderT`

``` purescript
withReaderT :: forall r1 r2 m a b. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
```

Change the type of the context in a `ReaderT` monad action.

#### `mapReaderT`

``` purescript
mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b
```

Change the type of the result in a `ReaderT` monad action.

#### `functorReaderT`

``` purescript
instance functorReaderT :: (Functor m) => Functor (ReaderT r m)
```


#### `applyReaderT`

``` purescript
instance applyReaderT :: (Applicative m) => Apply (ReaderT r m)
```


#### `applicativeReaderT`

``` purescript
instance applicativeReaderT :: (Applicative m) => Applicative (ReaderT r m)
```


#### `altReaderT`

``` purescript
instance altReaderT :: (Alt m) => Alt (ReaderT r m)
```


#### `plusReaderT`

``` purescript
instance plusReaderT :: (Plus m) => Plus (ReaderT r m)
```


#### `alternativeReaderT`

``` purescript
instance alternativeReaderT :: (Alternative m) => Alternative (ReaderT r m)
```


#### `bindReaderT`

``` purescript
instance bindReaderT :: (Monad m) => Bind (ReaderT r m)
```


#### `monadReaderT`

``` purescript
instance monadReaderT :: (Monad m) => Monad (ReaderT r m)
```


#### `monadPlusReaderT`

``` purescript
instance monadPlusReaderT :: (MonadPlus m) => MonadPlus (ReaderT r m)
```


#### `monadTransReaderT`

``` purescript
instance monadTransReaderT :: MonadTrans (ReaderT r)
```


#### `liftReaderT`

``` purescript
liftReaderT :: forall r m a. m a -> ReaderT r m a
```


#### `liftCatchReader`

``` purescript
liftCatchReader :: forall r m e a. (m a -> (e -> m a) -> m a) -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
```


#### `liftCallCCReader`

``` purescript
liftCallCCReader :: forall r m a b. (((a -> m b) -> m a) -> m a) -> ((a -> ReaderT r m b) -> ReaderT r m a) -> ReaderT r m a
```



## Module Control.Monad.Reader


This module defines the `Reader` monad.

#### `Reader`

``` purescript
type Reader r = ReaderT r Identity
```

The `Reader` monad is a synonym for the `ReaderT` monad transformer, applied
to the `Identity` monad.

#### `runReader`

``` purescript
runReader :: forall r a. Reader r a -> r -> a
```

Run a computation in the `Reader` monad.

#### `withReader`

``` purescript
withReader :: forall r1 r2 a b. (r2 -> r1) -> Reader r1 a -> Reader r2 a
```

Change the type of the context in a `Reader` monad action.

#### `mapReader`

``` purescript
mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
```

Change the type of the result in a `Reader` monad action.