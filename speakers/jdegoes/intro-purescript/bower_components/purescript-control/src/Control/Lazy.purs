-- | This module defines the `Lazy` type class and associated
-- | helper functions.

module Control.Lazy where

-- | The `Lazy` class represents types which allow evaluation of values
-- | to be _deferred_.
-- |
-- | Usually, this means that a type contains a function arrow which can
-- | be used to delay evaluation.
class Lazy l where
  defer :: (Unit -> l) -> l

-- | A version of `Lazy` for type constructors of one type argument.
class Lazy1 l where
  defer1 :: forall a. (Unit -> l a) -> l a

-- | A version of `Lazy` for type constructors of two type arguments.
class Lazy2 l where
  defer2 :: forall a b. (Unit -> l a b) -> l a b

-- | `fix` defines a value as the fixed point of a function.
-- |
-- | The `Lazy` instance allows us to generate the result lazily.
fix :: forall l a. (Lazy l) => (l -> l) -> l
fix f = defer (\_ -> f (fix f))

-- | A version of `fix` for type constructors of one type argument.
fix1 :: forall l a. (Lazy1 l) => (l a -> l a) -> l a
fix1 f = defer1 (\_ -> f (fix1 f))

-- | A version of `fix` for type constructors of two type arguments.
fix2 :: forall l a b. (Lazy2 l) => (l a b -> l a b) -> l a b
fix2 f = defer2 (\_ -> f (fix2 f))
