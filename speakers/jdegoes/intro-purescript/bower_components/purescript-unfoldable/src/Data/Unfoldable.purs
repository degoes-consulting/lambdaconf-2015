-- | This module provides a type class for _unfoldable functors_, i.e.
-- | functors which support an `unfoldr` operation.
-- |
-- | This allows us to unify various operations on arrays, lists,
-- | sequences, etc.

module Data.Unfoldable where

import Data.Maybe
import Data.Tuple
import Data.Array.ST
import Control.Monad.Eff
import Control.Monad.ST

-- | This class identifies data structures which can be _unfolded_,
-- | generalizing `unfoldr` on arrays.
-- |
-- | The generating function `f` in `unfoldr f` in understood as follows:
-- |
-- | - If `f b` is `Nothing`, then `unfoldr f b` should be empty.
-- | - If `f b` is `Just (Tuple a b1)`, then `unfoldr f b` should consist of `a`
-- |   appended to the result of `unfoldr f b1`.
class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a
 
instance unfoldableArray :: Unfoldable [] where
  unfoldr f b = runPure (runSTArray (do
    arr  <- emptySTArray
    seed <- newSTRef b
    untilE $ do
      b1 <- readSTRef seed
      case f b1 of
        Nothing -> return true
        Just (Tuple a b2) -> do
          pushSTArray arr a
          writeSTRef seed b2
          return false
    return arr))
