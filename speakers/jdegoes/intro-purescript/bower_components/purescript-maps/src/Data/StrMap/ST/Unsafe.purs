module Data.StrMap.ST.Unsafe
  ( unsafeGet
  ) where

import Control.Monad.Eff (Eff())
import Control.Monad.ST (ST())
import Data.StrMap (StrMap())
import Data.StrMap.ST (STStrMap())

-- | Unsafely get the value for a key in a map.
-- |
-- | This function does not check whether the key exists in the map.
foreign import unsafeGet
  """
  function unsafeGet(m) {
    return function() {
      return m;
    }
  }
  """ :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)
