-- | Helper functions for working with mutable maps using the `ST` effect.
-- |
-- | This module can be used when performance is important and mutation is a local effect.

module Data.StrMap.ST
  ( STStrMap()
  , new
  , peek
  , poke
  , delete
  ) where

import Control.Monad.Eff
import Control.Monad.ST
import Data.Maybe

-- | A reference to a mutable map
-- |
-- | The first type parameter represents the memory region which the map belongs to. The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STStrMap h a` is the same as that of `StrMap a`, except that mutation is allowed.
foreign import data STStrMap :: * -> * -> *

foreign import _new
  """
  function _new() {
    return {};
  }
  """ :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)

-- | Create a new, empty mutable map
new :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)
new = _new

-- | Get the value for a key in a mutable map
foreign import peek
  """
  function peek(m) {
    return function(k) {
      return function() {
        return m[k];
      }
    }
  }
  """ :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) a

-- | Update the value for a key in a mutable map
foreign import poke
  """
  function poke(m) {
    return function(k) {
      return function(v) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }
  """ :: forall a h r. STStrMap h a -> String -> a -> Eff (st :: ST h | r) (STStrMap h a)

foreign import _delete
  """
  function _delete(m) {
    return function(k) {
      return function() {
        delete m[k];
        return m;
      };
    };
  }
  """ :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (STStrMap h a)

-- | Remove a key and the corresponding value from a mutable map
delete :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (STStrMap h a)
delete = _delete
