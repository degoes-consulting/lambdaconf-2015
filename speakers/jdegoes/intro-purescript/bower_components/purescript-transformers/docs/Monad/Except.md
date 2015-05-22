# Module Documentation

## Module Control.Monad.Except.Trans

#### `ExceptT`

``` purescript
newtype ExceptT e m a
  = ExceptT (m (Either e a))
```

A monad transformer which adds exceptions to other monads, in the same way
as `Except`. As before, `e` is the type of exceptions, and `a` is the type
of successful results. The new type parameter `m` is the inner monad that
computations run in.

#### `runExceptT`

``` purescript
runExceptT :: forall e m a. ExceptT e m a -> m (Either e a)
```

The inverse of `ExceptT`. Run a computation in the `ExceptT` monad.

#### `withExceptT`

``` purescript
withExceptT :: forall e e' m a. (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
```

Transform any exceptions thrown by an `ExceptT` computation using the given function.

#### `mapExceptT`

``` purescript
mapExceptT :: forall e e' m n a b. (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
```

Transform the unwrapped computation using the given function.

#### `functorExceptT`

``` purescript
instance functorExceptT :: (Functor f) => Functor (ExceptT e f)
```


#### `applyExceptT`

``` purescript
instance applyExceptT :: (Apply f) => Apply (ExceptT e f)
```


#### `applicativeExceptT`

``` purescript
instance applicativeExceptT :: (Monad m) => Applicative (ExceptT e m)
```


#### `bindExceptT`

``` purescript
instance bindExceptT :: (Monad m) => Bind (ExceptT e m)
```


#### `monadExceptT`

``` purescript
instance monadExceptT :: (Monad m) => Monad (ExceptT e m)
```


#### `altExceptT`

``` purescript
instance altExceptT :: (Semigroup e, Monad m) => Alt (ExceptT e m)
```


#### `plusExceptT`

``` purescript
instance plusExceptT :: (Monoid e, Monad m) => Plus (ExceptT e m)
```


#### `alternativeExceptT`

``` purescript
instance alternativeExceptT :: (Monoid e, Monad m) => Alternative (ExceptT e m)
```


#### `monadPlusExceptT`

``` purescript
instance monadPlusExceptT :: (Monoid e, Monad m) => MonadPlus (ExceptT e m)
```


#### `throwE`

``` purescript
throwE :: forall e m a. (Applicative m) => e -> ExceptT e m a
```

Throw an exception in an `ExceptT` computation.

#### `catchE`

``` purescript
catchE :: forall e e' m a. (Monad m) => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
```

Catch an exception in an `ExceptT` computation.


## Module Control.Monad.Except

#### `Except`

``` purescript
type Except e a = ExceptT e Identity a
```

A parametrizable exception monad; computations are either exceptions or
pure values. If an exception is thrown (see `throwE`), the computation
terminates with that value. Exceptions may also be caught with `catchE`,
allowing the computation to resume and exit successfully.

The type parameter `e` is the type of exceptions, and `a` is the type
of successful results.

A mechanism for trying many different computations until one succeeds is
provided via the `Alt` instance, specifically the `(<|>)` function.
The first computation to succeed is returned; if all fail, the exceptions
are combined using their `Semigroup` instance. The `Plus` instance goes
further and adds the possibility of a computation failing with an 'empty'
exception; naturally, this requires the stronger constraint of a `Monoid`
instance for the exception type.

#### `except`

``` purescript
except :: forall e a. Either e a -> Except e a
```

Construct a computation in the `Except` monad from an `Either` value.

#### `runExcept`

``` purescript
runExcept :: forall e a. Except e a -> Either e a
```

Run a computation in the `Except` monad. The inverse of `except`.

#### `mapExcept`

``` purescript
mapExcept :: forall e e' a b. (Either e a -> Either e' b) -> Except e a -> Except e' b
```

Transform the unwrapped computation using the given function.

#### `withExcept`

``` purescript
withExcept :: forall e e' a. (e -> e') -> Except e a -> Except e' a
```

Transform any exceptions thrown by an `Except` computation using the given function.