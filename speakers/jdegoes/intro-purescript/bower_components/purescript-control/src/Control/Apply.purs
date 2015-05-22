-- | This module defines helper functions for working with `Apply` instances.

module Control.Apply where

  infixl 4 <*
  infixl 4 *>

  -- | Combine two effectful actions, keeping only the result of the first.
  (<*) :: forall a b f. (Apply f) => f a -> f b -> f a
  (<*) a b = const <$> a <*> b

  -- | Combine two effectful actions, keeping only the result of the second.
  (*>) :: forall a b f. (Apply f) => f a -> f b -> f b
  (*>) a b = const id <$> a <*> b

  -- | Lift a function of two arguments to a function which accepts and returns
  -- | values wrapped with the type constructor `f`.
  lift2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
  lift2 f a b = f <$> a <*> b

  -- | Lift a function of three arguments to a function which accepts and returns
  -- | values wrapped with the type constructor `f`.
  lift3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  lift3 f a b c = f <$> a <*> b <*> c

  -- | Lift a function of four arguments to a function which accepts and returns
  -- | values wrapped with the type constructor `f`.
  lift4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
  lift4 f a b c d = f <$> a <*> b <*> c <*> d

  -- | Lift a function of five arguments to a function which accepts and returns
  -- | values wrapped with the type constructor `f`.
  lift5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
  lift5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
