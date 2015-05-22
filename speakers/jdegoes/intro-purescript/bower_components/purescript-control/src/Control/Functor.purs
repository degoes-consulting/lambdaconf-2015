-- | This module defines helper functions for working with `Functor` instances.

module Control.Functor where

infixl 4 <$
infixl 4 $>

-- | Ignore the return value of a computation, using the specified return value instead.
(<$) :: forall f a b. (Functor f) => a -> f b -> f a
(<$) x f = const x <$> f

-- | A version of `(<$)` with its arguments flipped.
($>) :: forall f a b. (Functor f) => f a -> b -> f b
($>) f x = const x <$> f
