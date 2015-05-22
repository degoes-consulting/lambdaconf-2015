# Module Documentation

## Module Control.Monad.Writer.Class


This module defines the `MonadWriter` type class and its instances.

#### `MonadWriter`

``` purescript
class MonadWriter w m where
  writer :: forall a. Tuple a w -> m a
  listen :: forall a. m a -> m (Tuple a w)
  pass :: forall a. m (Tuple a (w -> w)) -> m a
```

The `MonadWriter w` type class represents those monads which support a monoidal accumulator
of type `w`.

- `writer` appends a value to the accumulator.
- `listen` modifies the result to include the changes to the accumulator.
- `pass` applies the returned function to the accumulator.

An implementation is provided for `WriterT`, and for other monad transformers
defined in this library.

Laws:

- `writer a mempty = pure a`
- `do { tell x ; tell y } = tell (x <> y)`
- `listen (pure a) = pure (Tuple a mempty)`
- `listen (writer a x) = tell x $> Tuple a x`


#### `tell`

``` purescript
tell :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => w -> m Unit
```

Append a value to the accumulator.

#### `listens`

``` purescript
listens :: forall w m a b. (Monoid w, Monad m, MonadWriter w m) => (w -> b) -> m a -> m (Tuple a b)
```

Read a value which depends on the modifications made to the accumulator during an action.

#### `censor`

``` purescript
censor :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => (w -> w) -> m a -> m a
```

Modify the final accumulator value by applying a function.

#### `monadWriterWriterT`

``` purescript
instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m)
```


#### `monadWriterErrorT`

``` purescript
instance monadWriterErrorT :: (Monad m, MonadWriter w m) => MonadWriter w (ErrorT e m)
```


#### `monadWriterMaybeT`

``` purescript
instance monadWriterMaybeT :: (Monad m, MonadWriter w m) => MonadWriter w (MaybeT m)
```


#### `monadWriterStateT`

``` purescript
instance monadWriterStateT :: (Monad m, MonadWriter w m) => MonadWriter w (StateT s m)
```


#### `monadWriterReaderT`

``` purescript
instance monadWriterReaderT :: (Monad m, MonadWriter w m) => MonadWriter w (ReaderT r m)
```


#### `monadWriterRWST`

``` purescript
instance monadWriterRWST :: (Monad m, Monoid w) => MonadWriter w (RWST r w s m)
```



## Module Control.Monad.Writer.Trans


This module defines the writer monad transformer, `WriterT`.

#### `WriterT`

``` purescript
newtype WriterT w m a
  = WriterT (m (Tuple a w))
```

The writer monad transformer.

This monad transformer extends the base monad with a monoidal accumulator of
type `w`.

The `MonadWriter` type class describes the operations supported by this monad.

#### `runWriterT`

``` purescript
runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)
```

Run a computation in the `WriterT` monad.

#### `execWriterT`

``` purescript
execWriterT :: forall w m a. (Apply m) => WriterT w m a -> m w
```

Run a computation in the `WriterT` monad, discarding the result.

#### `mapWriterT`

``` purescript
mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b
```

Change the accumulator and base monad types in a `WriterT` monad action.

#### `functorWriterT`

``` purescript
instance functorWriterT :: (Functor m) => Functor (WriterT w m)
```


#### `applyWriterT`

``` purescript
instance applyWriterT :: (Monoid w, Apply m) => Apply (WriterT w m)
```


#### `applicativeWriterT`

``` purescript
instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m)
```


#### `altWriterT`

``` purescript
instance altWriterT :: (Monoid w, Alt m) => Alt (WriterT w m)
```


#### `plusWriterT`

``` purescript
instance plusWriterT :: (Monoid w, Plus m) => Plus (WriterT w m)
```


#### `alternativeWriterT`

``` purescript
instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)
```


#### `bindWriterT`

``` purescript
instance bindWriterT :: (Monoid w, Monad m) => Bind (WriterT w m)
```


#### `monadWriterT`

``` purescript
instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)
```


#### `monadPlusWriterT`

``` purescript
instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)
```


#### `monadTransWriterT`

``` purescript
instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w)
```


#### `liftCatchWriter`

``` purescript
liftCatchWriter :: forall w m e a. (m (Tuple a w) -> (e -> m (Tuple a w)) -> m (Tuple a w)) -> WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a
```


#### `liftCallCCWriter`

``` purescript
liftCallCCWriter :: forall w m a b. (Monoid w) => (((Tuple a w -> m (Tuple b w)) -> m (Tuple a w)) -> m (Tuple a w)) -> ((a -> WriterT w m b) -> WriterT w m a) -> WriterT w m a
```