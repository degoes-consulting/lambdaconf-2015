# Module Documentation

## Module Control.Monad.Maybe.Trans


This module defines the `MaybeT` monad transformer.

#### `MaybeT`

``` purescript
newtype MaybeT m a
  = MaybeT (m (Maybe a))
```

The `MaybeT` monad transformer.

This monad transformer extends the base monad, supporting failure and alternation via
the `MonadPlus` type class.

#### `runMaybeT`

``` purescript
runMaybeT :: forall m a. MaybeT m a -> m (Maybe a)
```

Run a computation in the `MaybeT` monad.

#### `mapMaybeT`

``` purescript
mapMaybeT :: forall m1 m2 a b. (m1 (Maybe a) -> m2 (Maybe b)) -> MaybeT m1 a -> MaybeT m2 b
```

Change the result type of a `MaybeT` monad action.

#### `functorMaybeT`

``` purescript
instance functorMaybeT :: (Monad m) => Functor (MaybeT m)
```


#### `applyMaybeT`

``` purescript
instance applyMaybeT :: (Monad m) => Apply (MaybeT m)
```


#### `applicativeMaybeT`

``` purescript
instance applicativeMaybeT :: (Monad m) => Applicative (MaybeT m)
```


#### `bindMaybeT`

``` purescript
instance bindMaybeT :: (Monad m) => Bind (MaybeT m)
```


#### `monadMaybeT`

``` purescript
instance monadMaybeT :: (Monad m) => Monad (MaybeT m)
```


#### `monadTransMaybeT`

``` purescript
instance monadTransMaybeT :: MonadTrans MaybeT
```


#### `altMaybeT`

``` purescript
instance altMaybeT :: (Monad m) => Alt (MaybeT m)
```


#### `plusMaybeT`

``` purescript
instance plusMaybeT :: (Monad m) => Plus (MaybeT m)
```


#### `alternativeMaybeT`

``` purescript
instance alternativeMaybeT :: (Monad m) => Alternative (MaybeT m)
```


#### `monadPlusMaybeT`

``` purescript
instance monadPlusMaybeT :: (Monad m) => MonadPlus (MaybeT m)
```


#### `liftCatchMaybe`

``` purescript
liftCatchMaybe :: forall m e a. (m (Maybe a) -> (e -> m (Maybe a)) -> m (Maybe a)) -> MaybeT m a -> (e -> MaybeT m a) -> MaybeT m a
```


#### `liftListenMaybe`

``` purescript
liftListenMaybe :: forall m a w. (Monad m) => (m (Maybe a) -> m (Tuple (Maybe a) w)) -> MaybeT m a -> MaybeT m (Tuple a w)
```


#### `liftPassMaybe`

``` purescript
liftPassMaybe :: forall m a w. (Monad m) => (m (Tuple (Maybe a) (w -> w)) -> m (Maybe a)) -> MaybeT m (Tuple a (w -> w)) -> MaybeT m a
```


#### `liftCallCCMaybe`

``` purescript
liftCallCCMaybe :: forall m a b. (((Maybe a -> m (Maybe b)) -> m (Maybe a)) -> m (Maybe a)) -> ((a -> MaybeT m b) -> MaybeT m a) -> MaybeT m a
```