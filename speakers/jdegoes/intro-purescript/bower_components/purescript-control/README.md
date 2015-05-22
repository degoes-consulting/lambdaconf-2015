# Module Documentation

## Module Control.Alt


This module defines the `Alt` type class.

#### `Alt`

``` purescript
class (Functor f) <= Alt f where
  (<|>) :: forall a. f a -> f a -> f a
```

The `Alt` type class identifies an associative operation on a type
constructor.  It is similar to `Semigroup`, except that it applies to
types of kind `* -> *`, like `Array` or `List`, rather than concrete types
`String` or `Number`.

`Alt` instances are required to satisfy the following laws:

- Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
- Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`

For example, the `Array` (`[]`) type is an instance of `Alt`, where
`(<|>)` is defined to be concatenation.


## Module Control.Alternative


This module defines the `Alternative` type class and associated
helper functions.

#### `Alternative`

``` purescript
class (Applicative f, Plus f) <= Alternative f where
```

The `Alternative` type class has no members of its own; it just specifies
that the type constructor has both `Applicative` and `Plus` instances.

Types which have `Alternative` instances should also satisfy the following
laws:

- Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
- Annihilation: `empty <*> f = empty`

#### `some`

``` purescript
some :: forall f a. (Alternative f, Lazy1 f) => f a -> f [a]
```

Attempt a computation multiple times, requiring at least one success.

The `Lazy` constraint is used to generate the result lazily, to ensure
termination.

#### `many`

``` purescript
many :: forall f a. (Alternative f, Lazy1 f) => f a -> f [a]
```

Attempt a computation multiple times, returning as many successful results
as possible (possibly zero).

The `Lazy` constraint is used to generate the result lazily, to ensure
termination.


## Module Control.Apply


This module defines helper functions for working with `Apply` instances.

#### `(<*)`

``` purescript
(<*) :: forall a b f. (Apply f) => f a -> f b -> f a
```

Combine two effectful actions, keeping only the result of the first.

#### `(*>)`

``` purescript
(*>) :: forall a b f. (Apply f) => f a -> f b -> f b
```

Combine two effectful actions, keeping only the result of the second.

#### `lift2`

``` purescript
lift2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
```

Lift a function of two arguments to a function which accepts and returns
values wrapped with the type constructor `f`.

#### `lift3`

``` purescript
lift3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

Lift a function of three arguments to a function which accepts and returns
values wrapped with the type constructor `f`.

#### `lift4`

``` purescript
lift4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
```

Lift a function of four arguments to a function which accepts and returns
values wrapped with the type constructor `f`.

#### `lift5`

``` purescript
lift5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
```

Lift a function of five arguments to a function which accepts and returns
values wrapped with the type constructor `f`.


## Module Control.Bind


This module defines helper functions for working with `Bind` instances.

#### `(=<<)`

``` purescript
(=<<) :: forall a b m. (Bind m) => (a -> m b) -> m a -> m b
```

A version of `(>>=)` with its arguments flipped.

#### `(>=>)`

``` purescript
(>=>) :: forall a b c m. (Bind m) => (a -> m b) -> (b -> m c) -> a -> m c
```

Forwards Kleisli composition.

For example:

```purescript
import Data.Array (head, tail)

third = tail >=> tail >=> head
```

#### `(<=<)`

``` purescript
(<=<) :: forall a b c m. (Bind m) => (b -> m c) -> (a -> m b) -> a -> m c
```

Backwards Kleisli composition.

#### `join`

``` purescript
join :: forall a m. (Bind m) => m (m a) -> m a
```

Collapse two applications of a monadic type constructor into one.

#### `ifM`

``` purescript
ifM :: forall a m. (Bind m) => m Boolean -> m a -> m a -> m a
```

Execute a monadic action if a condition holds. 

For example:

```purescript
main = ifM ((< 0.5) <$> random)
         (trace "Heads")
         (trace "Tails")
```


## Module Control.Comonad


This module defines the `Comonad` type class.

#### `Comonad`

``` purescript
class (Extend w) <= Comonad w where
  extract :: forall a. w a -> a
```

`Comonad` extends the `Extend` class with the `extract` function
which extracts a value, discarding the comonadic context.

`Comonad` is the dual of `Monad`, and `extract` is the dual of 
`pure` or `return`.

Laws:

- Left Identity: `extract <<= xs = xs`
- Right Identity: `extract (f <<= xs) = f xs`


## Module Control.Extend


This module defines the `Extend` type class and associated helper functions.

#### `Extend`

``` purescript
class (Functor w) <= Extend w where
  (<<=) :: forall b a. (w a -> b) -> w a -> w b
```

The `Extend` class defines the extension operator `(<<=)`
which extends a local context-dependent computation to
a global computation.

`Extend` is the dual of `Bind`, and `(<<=)` is the dual of 
`(>>=)`.

Laws:

- Associativity: `extend f <<< extend g = extend (f <<< extend g)`

#### `extendArr`

``` purescript
instance extendArr :: (Semigroup w) => Extend (Prim.Function w)
```


#### `(=>>)`

``` purescript
(=>>) :: forall b a w. (Extend w) => w a -> (w a -> b) -> w b
```

A version of `(<<=)` with its arguments flipped.

#### `(=>=)`

``` purescript
(=>=) :: forall b a w c. (Extend w) => (w a -> b) -> (w b -> c) -> w a -> c
```

Forwards co-Kleisli composition.

#### `(=<=)`

``` purescript
(=<=) :: forall b a w c. (Extend w) => (w b -> c) -> (w a -> b) -> w a -> c
```

Backwards co-Kleisli composition.

#### `extend`

``` purescript
extend :: forall b a w. (Extend w) => (w a -> b) -> w a -> w b
```

An alias for `(<<=)`.

#### `duplicate`

``` purescript
duplicate :: forall a w. (Extend w) => w a -> w (w a)
```

Duplicate a comonadic context.

`duplicate` is dual to `Control.Bind.join`.


## Module Control.Functor


This module defines helper functions for working with `Functor` instances.

#### `(<$)`

``` purescript
(<$) :: forall f a b. (Functor f) => a -> f b -> f a
```

Ignore the return value of a computation, using the specified return value instead.

#### `($>)`

``` purescript
($>) :: forall f a b. (Functor f) => f a -> b -> f b
```

A version of `(<$)` with its arguments flipped.


## Module Control.Lazy


This module defines the `Lazy` type class and associated
helper functions.

#### `Lazy`

``` purescript
class Lazy l where
  defer :: (Unit -> l) -> l
```

The `Lazy` class represents types which allow evaluation of values
to be _deferred_.

Usually, this means that a type contains a function arrow which can
be used to delay evaluation.

#### `Lazy1`

``` purescript
class Lazy1 l where
  defer1 :: forall a. (Unit -> l a) -> l a
```

A version of `Lazy` for type constructors of one type argument.

#### `Lazy2`

``` purescript
class Lazy2 l where
  defer2 :: forall a b. (Unit -> l a b) -> l a b
```

A version of `Lazy` for type constructors of two type arguments.

#### `fix`

``` purescript
fix :: forall l a. (Lazy l) => (l -> l) -> l
```

`fix` defines a value as the fixed point of a function.

The `Lazy` instance allows us to generate the result lazily.

#### `fix1`

``` purescript
fix1 :: forall l a. (Lazy1 l) => (l a -> l a) -> l a
```

A version of `fix` for type constructors of one type argument.

#### `fix2`

``` purescript
fix2 :: forall l a b. (Lazy2 l) => (l a b -> l a b) -> l a b
```

A version of `fix` for type constructors of two type arguments.


## Module Control.Monad


This module defines helper functions for working with `Monad` instances.

#### `replicateM`

``` purescript
replicateM :: forall m a. (Monad m) => Number -> m a -> m [a]
```

Perform a monadic action `n` times collecting all of the results.

#### `foldM`

``` purescript
foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```

Perform a fold using a monadic step function.

#### `when`

``` purescript
when :: forall m. (Monad m) => Boolean -> m Unit -> m Unit
```

Perform a monadic action when a condition is true.

#### `unless`

``` purescript
unless :: forall m. (Monad m) => Boolean -> m Unit -> m Unit
```

Perform a monadic action unless a condition is true.

#### `filterM`

``` purescript
filterM :: forall a m. (Monad m) => (a -> m Boolean) -> [a] -> m [a]
```

Filter where the predicate returns a monadic `Boolean`.

For example: 

```purescript
powerSet :: forall a. [a] -> [[a]]
powerSet = filterM (const [true, false])
```


## Module Control.MonadPlus


This module defines the `MonadPlus` type class.

#### `MonadPlus`

``` purescript
class (Monad m, Alternative m) <= MonadPlus m where
```

The `MonadPlus` type class has no members of its own; it just specifies
that the type has both `Monad` and `Alternative` instances.

Types which have `MonadPlus` instances should also satisfy the following
laws:

- Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
- Annihilation: `empty >>= f = empty`

#### `guard`

``` purescript
guard :: forall m. (MonadPlus m) => Boolean -> m Unit
```

Fail using `Plus` if a condition does not hold, or
succeed using `Monad` if it does.

For example:

```purescript
import Data.Array

factors :: Number -> [Number]
factors n = do
  a <- 1 .. n
  b <- 1 .. a
  guard $ a * b == n
  return a
```


## Module Control.Plus


This module defines the `Plus` type class.

#### `Plus`

``` purescript
class (Alt f) <= Plus f where
  empty :: forall a. f a
```

The `Plus` type class extends the `Alt` type class with a value that
should be the left and right identity for `(<|>)`.

It is similar to `Monoid`, except that it applies to types of
kind `* -> *`, like `Array` or `List`, rather than concrete types like
`String` or `Number`.

`Plus` instances should satisfy the following laws:

- Left identity: `empty <|> x == x`
- Right identity: `x <|> empty == x`
- Annihilation: `f <$> empty == empty`