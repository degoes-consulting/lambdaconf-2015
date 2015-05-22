# Module Documentation

## Module Control.Comonad.Traced.Class


This module defines the `ComonadTraced` type class and its instances.

#### `ComonadTraced`

``` purescript
class (Comonad w) <= ComonadTraced t w where
  track :: forall a. t -> w a -> a
```

The `ComonadTraced` type class represents those monads which support relative (monoidal)
position information via `track`.

- `track` extracts a value at the specified relative position.

An implementation is provided for `TracedT`.

Laws:

- `track mempty = extract`
- `track s <<= track t x = track (s <> t) x`

For example:

```purescript
blur :: forall w. (ComonadTraced (Additive Number) w) -> w Number -> w Number
blur = extend \r -> (track (Additive (-1)) r + track (Additive 1) r) / 2
```

#### `tracks`

``` purescript
tracks :: forall w a t. (Comonad w, ComonadTraced t w) => (a -> t) -> w a -> a
```

Extracts a value at a relative position which depends on the current value.

#### `listen`

``` purescript
listen :: forall w a t. (Functor w) => TracedT t w a -> TracedT t w (Tuple a t)
```

Get the current position.

#### `listens`

``` purescript
listens :: forall w a t b. (Functor w) => (t -> b) -> TracedT t w a -> TracedT t w (Tuple a b)
```

Get a value which depends on the current position.

#### `censor`

``` purescript
censor :: forall w a t b. (Functor w) => (t -> t) -> TracedT t w a -> TracedT t w a
```

Apply a function to the current position.

#### `comonadTracedTracedT`

``` purescript
instance comonadTracedTracedT :: (Comonad w, Monoid t) => ComonadTraced t (TracedT t w)
```



## Module Control.Comonad.Traced.Trans


This module defines the cowriter comonad transformer, `TracedT`.

#### `TracedT`

``` purescript
newtype TracedT t w a
  = TracedT (w (t -> a))
```

The cowriter comonad transformer.

This comonad transformer extends the context of a value in the base comonad so that the value
depends on a monoidal position of type `t`.

The `ComonadTraced` type class describes the operations supported by this comonad.

#### `runTracedT`

``` purescript
runTracedT :: forall w a t. TracedT t w a -> w (t -> a)
```

Unwrap a value in the `TracedT` comonad.

#### `functorTracedT`

``` purescript
instance functorTracedT :: (Functor w) => Functor (TracedT t w)
```


#### `extendTracedT`

``` purescript
instance extendTracedT :: (Extend w, Semigroup t) => Extend (TracedT t w)
```


#### `comonadTracedT`

``` purescript
instance comonadTracedT :: (Comonad w, Monoid t) => Comonad (TracedT t w)
```


#### `comonadTransTracedT`

``` purescript
instance comonadTransTracedT :: (Monoid t) => ComonadTrans (TracedT t)
```



## Module Control.Comonad.Traced


This module defines the `Traced` comonad.

#### `Traced`

``` purescript
type Traced m = TracedT m Identity
```

The `Traced` comonad is a synonym for the `TracedT` comonad transformer, applied
to the `Identity` monad.

#### `runTraced`

``` purescript
runTraced :: forall m a. Traced m a -> m -> a
```

Unwrap a value in the `Traced` comonad.

#### `traced`

``` purescript
traced :: forall m a. (m -> a) -> Traced m a
```

Create a value in context in the `Traced` comonad.