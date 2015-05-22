-- | This module defines the `ComonadEnv` type class and its instances.

module Control.Comonad.Env.Class where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Env.Trans

import Data.Tuple

-- | The `ComonadEnv` type class represents those monads which support a global environment via
-- | `ask` and `local`.
-- |
-- | - `ask` reads the current environment from the context.
-- | - `local` changes the value of the global environment.
-- |
-- | An implementation is provided for `EnvT`.
-- |
-- | Laws:
-- |
-- | - `ask (local f x) = f (ask x)`
-- | - `extract (local _ x) = extract a`
-- | - `extend g (local f x) = extend (g <<< local f) x` 
class (Comonad w) <= ComonadEnv e w where
  ask :: forall a. w a -> e
  local :: forall a. (e -> e) -> w a -> w a
  
-- | Get a value which depends on the environment.
asks :: forall e1 e2 w a. (ComonadEnv e1 w) => (e1 -> e2) -> w e1 -> e2
asks f x = f $ ask x

instance comonadEnvTuple :: ComonadEnv e (Tuple e) where
  ask = fst
  local f (Tuple x y) = Tuple (f x) y

instance comonadEnvEnvT :: (Comonad w) => ComonadEnv e (EnvT e w) where
  ask x = fst $ runEnvT x
  local f x = EnvT $ case runEnvT x of
    Tuple x y -> Tuple (f x) y
