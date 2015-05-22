-- | This module defines the `ComonadStore` type class and its instances.

module Control.Comonad.Store.Class where

import Control.Comonad
import Control.Comonad.Store.Trans
import Control.Extend

import Data.Tuple

-- | The `ComonadStore` type class represents those monads which support local position information via
-- | `pos` and `peek`.
-- |
-- | - `pos` reads the current position.
-- | - `peek` reads the value at the specified position in the specified context.
-- |
-- | An implementation is provided for `StoreT`.
-- |
-- | Laws:
-- |
-- | - `pos (extend _ x) = pos x`
-- | - `peek (pos x) x = extract x`
-- |
-- | For example:
-- |
-- | ```purescript
-- | blur :: forall w. (ComonadStore Number w) -> w Number -> w Number
-- | blur = extend \r -> (peeks (\n -> n - 1) r + peeks (\n -> n + 1) r) / 2)
-- | ```
class (Comonad w) <= ComonadStore s w where
  pos :: forall a. w a -> s
  peek :: forall a. s -> w a -> a

-- | Extract a collection of values from positions which depend on the current position.
experiment :: forall f a w s. (ComonadStore s w, Functor f) => (s -> f s) -> w a -> f a
experiment f x = flip peek x <$> f (pos x)

-- | Extract a value from a position which depends on the current position.
peeks :: forall s a w. (ComonadStore s w) => (s -> s) -> w a -> a
peeks f x = peek (f $ pos x) x

-- | Reposition the focus at the specified position.
seek :: forall s a w. (ComonadStore s w, Extend w) => s -> w a -> w a
seek s x = peek s $ duplicate x

-- | Reposition the focus at the specified position, which depends on the current position.
seeks :: forall s a w. (ComonadStore s w, Extend w) => (s -> s) -> w a -> w a
seeks f x = peeks f $ duplicate x

instance comonadStoreStoreT :: (Comonad w) => ComonadStore s (StoreT s w) where
  pos (StoreT (Tuple f s)) = s
  peek s (StoreT (Tuple f _)) = extract f s