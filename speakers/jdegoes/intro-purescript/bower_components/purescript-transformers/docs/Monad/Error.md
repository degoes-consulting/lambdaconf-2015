# Module Documentation

## Module Control.Monad.Error.Class


This module defines the `MonadError` type class and its instances.

#### `MonadError`

``` purescript
class MonadError e m where
  throwError :: forall a. e -> m a
  catchError :: forall a. m a -> (e -> m a) -> m a
```

The `MonadError` type class represents those monads which support errors via
`throwError` and `catchError`.

- `throwError e` throws the error `e`
- `catchError x f` calls the error handler `f` if an error is thrown during the
  evaluation of `x`.

An implementation is provided for `ErrorT`, and for other monad transformers
defined in this library.

Laws:

- Left zero: `throwError e >>= f = throwError e`
- Catch: `catchError (throwError e) f = f e`
- Pure: `catchError (pure a) f = pure a`


#### `catchJust`

``` purescript
catchJust :: forall e m a b. (MonadError e m) => (e -> Maybe b) -> m a -> (b -> m a) -> m a
```

This function allows you to provide a predicate for selecting the
exceptions that you're interested in, and handle only those exceptons.
If the inner computation throws an exception, and the predicate returns
Nothing, then the whole computation will still fail with that exception.

#### `monadErrorEither`

``` purescript
instance monadErrorEither :: MonadError e (Either e)
```


#### `monadErrorMaybe`

``` purescript
instance monadErrorMaybe :: MonadError Unit Maybe
```


#### `monadErrorErrorT`

``` purescript
instance monadErrorErrorT :: (Monad m) => MonadError e (ErrorT e m)
```


#### `monadErrorExceptT`

``` purescript
instance monadErrorExceptT :: (Monad m) => MonadError e (ExceptT e m)
```


#### `monadErrorMaybeT`

``` purescript
instance monadErrorMaybeT :: (Monad m, MonadError e m) => MonadError e (MaybeT m)
```


#### `monadErrorReaderT`

``` purescript
instance monadErrorReaderT :: (Monad m, MonadError e m) => MonadError e (ReaderT r m)
```


#### `monadErrorWriterT`

``` purescript
instance monadErrorWriterT :: (Monad m, Monoid w, MonadError e m) => MonadError e (WriterT w m)
```


#### `monadErrorStateT`

``` purescript
instance monadErrorStateT :: (Monad m, MonadError e m) => MonadError e (StateT s m)
```



## Module Control.Monad.Error.Trans


This module defines the error monad transformer, `ErrorT`.

#### `ErrorT`

``` purescript
newtype ErrorT e m a
  = ErrorT (m (Either e a))
```

The error monad transformer

This monad transformer extends the base monad with the ability to throw and handle 
errors.

The `MonadError` type class describes the operations supported by this monad.

#### `runErrorT`

``` purescript
runErrorT :: forall e m a. ErrorT e m a -> m (Either e a)
```

Run a computation in the `ErrorT` monad.

#### `mapErrorT`

``` purescript
mapErrorT :: forall e1 e2 m1 m2 a b. (m1 (Either e1 a) -> m2 (Either e2 b)) -> ErrorT e1 m1 a -> ErrorT e2 m2 b
```

Change the error and result types in an `ErrorT` monad action.

#### `functorErrorT`

``` purescript
instance functorErrorT :: (Functor m) => Functor (ErrorT e m)
```


#### `applyErrorT`

``` purescript
instance applyErrorT :: (Apply m) => Apply (ErrorT e m)
```


#### `applicativeErrorT`

``` purescript
instance applicativeErrorT :: (Applicative m) => Applicative (ErrorT e m)
```


#### `altErrorT`

``` purescript
instance altErrorT :: (Monad m) => Alt (ErrorT e m)
```


#### `plusErrorT`

``` purescript
instance plusErrorT :: (Monad m, Error e) => Plus (ErrorT e m)
```


#### `alternativeErrorT`

``` purescript
instance alternativeErrorT :: (Monad m, Error e) => Alternative (ErrorT e m)
```


#### `bindErrorT`

``` purescript
instance bindErrorT :: (Monad m) => Bind (ErrorT e m)
```


#### `monadErrorT`

``` purescript
instance monadErrorT :: (Monad m) => Monad (ErrorT e m)
```


#### `monadPlusErrorT`

``` purescript
instance monadPlusErrorT :: (Monad m, Error e) => MonadPlus (ErrorT e m)
```


#### `monadTransErrorT`

``` purescript
instance monadTransErrorT :: MonadTrans (ErrorT e)
```


#### `liftListenError`

``` purescript
liftListenError :: forall e m a w. (Monad m) => (m (Either e a) -> m (Tuple (Either e a) w)) -> ErrorT e m a -> ErrorT e m (Tuple a w)
```


#### `liftPassError`

``` purescript
liftPassError :: forall e m a w. (Monad m) => (m (Tuple (Either e a) (w -> w)) -> m (Either e a)) -> ErrorT e m (Tuple a (w -> w)) -> ErrorT e m a
```


#### `liftCallCCError`

``` purescript
liftCallCCError :: forall e m a b. (((Either e a -> m (Either e b)) -> m (Either e a)) -> m (Either e a)) -> ((a -> ErrorT e m b) -> ErrorT e m a) -> ErrorT e m a
```



## Module Control.Monad.Error


This module defines the `Error` type class, which is used with the error monad
transformer, `ErrorT`.

#### `Error`

``` purescript
class Error a where
  noMsg :: a
  strMsg :: String -> a
```

The `Error` type class represents _error_ types, which can be 
constructed from error message strings.

#### `errorString`

``` purescript
instance errorString :: Error String
```