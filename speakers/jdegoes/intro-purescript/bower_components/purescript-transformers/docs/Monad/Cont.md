# Module Documentation

## Module Control.Monad.Cont.Class


This module defines the `MonadCont` type class and its instances.

#### `MonadCont`

``` purescript
class MonadCont m where
  callCC :: forall a b. ((a -> m b) -> m a) -> m a
```

The `MonadCont` type class represents those monads which support the
`callCC` operation.

An implementation is provided for `ContT`, and for other monad transformers
defined in this library.

#### `monadContContT`

``` purescript
instance monadContContT :: (Monad m) => MonadCont (Cont.ContT r m)
```


#### `monadContErrorT`

``` purescript
instance monadContErrorT :: (MonadCont m) => MonadCont (ErrorT e m)
```


#### `monadContMaybeT`

``` purescript
instance monadContMaybeT :: (MonadCont m) => MonadCont (MaybeT m)
```


#### `monadContReaderT`

``` purescript
instance monadContReaderT :: (MonadCont m) => MonadCont (ReaderT r m)
```


#### `monadContStateT`

``` purescript
instance monadContStateT :: (MonadCont m) => MonadCont (StateT s m)
```


#### `monadWriterT`

``` purescript
instance monadWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m)
```



## Module Control.Monad.Cont.Trans


This module defines the CPS monad transformer.

#### `ContT`

``` purescript
newtype ContT r m a
  = ContT ((a -> m r) -> m r)
```

The CPS monad transformer.

This monad transformer extends the base monad with the operation `callCC`.

#### `runContT`

``` purescript
runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
```

Run a computation in the `ContT` monad, by providing a continuation.

#### `mapContT`

``` purescript
mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a
```

Modify the underlying action in a `ContT` monad action.

#### `withContT`

``` purescript
withContT :: forall r m a b. ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b
```

Modify the continuation in a `ContT` monad action

#### `callCC`

``` purescript
callCC :: forall r m a b. ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
```

`callCC`, or _call-with-current-continuation_.

This action makes the current continuation available to the caller.

For example:

```purescript
delay :: forall eff. Number -> ContT Unit (Eff (timeout :: Timeout | eff)) Unit
delay n = callCC \cont -> 
  lift $ setTimeout n (runContT (cont unit) (\_ -> return unit))
```

#### `functorContT`

``` purescript
instance functorContT :: (Monad m) => Functor (ContT r m)
```


#### `applyContT`

``` purescript
instance applyContT :: (Functor m, Monad m) => Apply (ContT r m)
```


#### `applicativeContT`

``` purescript
instance applicativeContT :: (Functor m, Monad m) => Applicative (ContT r m)
```


#### `bindContT`

``` purescript
instance bindContT :: (Monad m) => Bind (ContT r m)
```


#### `monadContT`

``` purescript
instance monadContT :: (Monad m) => Monad (ContT r m)
```


#### `monadTransContT`

``` purescript
instance monadTransContT :: MonadTrans (ContT r)
```