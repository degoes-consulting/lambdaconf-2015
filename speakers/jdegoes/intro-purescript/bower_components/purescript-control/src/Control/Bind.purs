-- | This module defines helper functions for working with `Bind` instances.

module Control.Bind where

  infixr 1 =<<
  infixr 1 >=>
  infixr 1 <=<

  -- | A version of `(>>=)` with its arguments flipped.
  (=<<) :: forall a b m. (Bind m) => (a -> m b) -> m a -> m b
  (=<<) f m = m >>= f

  -- | Forwards Kleisli composition.
  -- |
  -- | For example:
  -- | 
  -- | ```purescript
  -- | import Data.Array (head, tail)
  -- | 
  -- | third = tail >=> tail >=> head
  -- | ```
  (>=>) :: forall a b c m. (Bind m) => (a -> m b) -> (b -> m c) -> a -> m c
  (>=>) f g a = f a >>= g

  -- | Backwards Kleisli composition.
  (<=<) :: forall a b c m. (Bind m) => (b -> m c) -> (a -> m b) -> a -> m c
  (<=<) f g a = f =<< g a

  -- | Collapse two applications of a monadic type constructor into one.
  join :: forall a m. (Bind m) => m (m a) -> m a
  join m = m >>= id

  -- | Execute a monadic action if a condition holds. 
  -- | 
  -- | For example:
  -- |
  -- | ```purescript
  -- | main = ifM ((< 0.5) <$> random)
  -- |          (trace "Heads")
  -- |          (trace "Tails")
  -- | ```
  ifM :: forall a m. (Bind m) => m Boolean -> m a -> m a -> m a
  ifM cond t f = cond >>= \cond' -> if cond' then t else f
