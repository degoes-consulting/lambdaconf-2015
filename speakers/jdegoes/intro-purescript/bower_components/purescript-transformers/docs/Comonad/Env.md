# Module Documentation

## Module Control.Comonad.Env.Class


This module defines the `ComonadEnv` type class and its instances.

#### `ComonadEnv`

``` purescript
class (Comonad w) <= ComonadEnv e w where
  ask :: forall a. w a -> e
  local :: forall a. (e -> e) -> w a -> w a
```

The `ComonadEnv` type class represents those monads which support a global environment via
`ask` and `local`.

- `ask` reads the current environment from the context.
- `local` changes the value of the global environment.

An implementation is provided for `EnvT`.

Laws:

- `ask (local f x) = f (ask x)`
- `extract (local _ x) = extract a`
- `extend g (local f x) = extend (g <<< local f) x` 

#### `asks`

``` purescript
asks :: forall e1 e2 w a. (ComonadEnv e1 w) => (e1 -> e2) -> w e1 -> e2
```

Get a value which depends on the environment.

#### `comonadEnvTuple`

``` purescript
instance comonadEnvTuple :: ComonadEnv e (Tuple e)
```


#### `comonadEnvEnvT`

``` purescript
instance comonadEnvEnvT :: (Comonad w) => ComonadEnv e (EnvT e w)
```



## Module Control.Comonad.Env.Trans


This module defines the environment comonad transformer, `EnvT`.

#### `EnvT`

``` purescript
newtype EnvT e w a
  = EnvT (Tuple e (w a))
```

The environment comonad transformer.

This comonad transformer extends the context of a value in the base comonad with a _global environment_ of
type `e`.

The `ComonadEnv` type class describes the operations supported by this comonad.

#### `runEnvT`

``` purescript
runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)
```

Unwrap a value in the `EnvT` comonad.

#### `withEnvT`

``` purescript
withEnvT :: forall e1 e2 w a. (e1 -> e2) -> EnvT e1 w a -> EnvT e2 w a
```

Change the environment type in an `EnvT` context.

#### `mapEnvT`

``` purescript
mapEnvT :: forall e w1 w2 a b. (w1 a -> w2 b) -> EnvT e w1 a -> EnvT e w2 b
```

Change the underlying comonad and data type in an `EnvT` context.

#### `functorEnvT`

``` purescript
instance functorEnvT :: (Functor w) => Functor (EnvT e w)
```


#### `extendEnvT`

``` purescript
instance extendEnvT :: (Extend w) => Extend (EnvT e w)
```


#### `comonadEnvT`

``` purescript
instance comonadEnvT :: (Comonad w) => Comonad (EnvT e w)
```


#### `comonadTransEnvT`

``` purescript
instance comonadTransEnvT :: ComonadTrans (EnvT e)
```



## Module Control.Comonad.Env


This module defines the `Env` comonad.

#### `Env`

``` purescript
type Env e = EnvT e Identity
```

The `Env` comonad is a synonym for the `EnvT` comonad transformer, applied
to the `Identity` monad.

#### `runEnv`

``` purescript
runEnv :: forall e a. Env e a -> Tuple e a
```

Unwrap a value in the `Env` comonad.

#### `withEnv`

``` purescript
withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a
```

Change the environment type in an `Env` computation.

#### `mapEnv`

``` purescript
mapEnv :: forall e a b. (a -> b) -> Env e a -> Env e b
```

Change the data type in an `Env` computation.

#### `env`

``` purescript
env :: forall e a. e -> a -> Env e a
```

Create a value in context in the `Env` comonad.