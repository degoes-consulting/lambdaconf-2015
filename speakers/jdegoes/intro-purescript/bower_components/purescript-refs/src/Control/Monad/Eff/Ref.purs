-- | This module defines an effect and actions for working with
-- | global mutable variables.
-- |
-- | _Note_: The `Control.Monad.ST` provides a _safe_ alternative
-- | to global mutable variables when mutation is restricted to a
-- | local scope.

module Control.Monad.Eff.Ref where

import Control.Monad.Eff

-- | The effect associated with the use of global mutable variables.
foreign import data Ref :: !

-- | A value of type `RefVal a` represents a mutable reference
-- | which holds a value of type `a`.
foreign import data RefVal :: * -> *

-- | Create a new mutable reference containing the specified value.
foreign import newRef """
  function newRef(val) {
    return function () {
      return { value: val };
    };
  }
""" :: forall s r. s -> Eff (ref :: Ref | r) (RefVal s)

-- | Read the current value of a mutable reference
foreign import readRef """
  function readRef(ref) {
    return function() {
      return ref.value;
    };
  }
""" :: forall s r. RefVal s -> Eff (ref :: Ref | r) s

-- | Update the value of a mutable reference by applying a function
-- | to the current value.
foreign import modifyRef' """
  function modifyRef$prime(ref) {
    return function(f) {
      return function() {
        var t = f(ref.value);
        ref.value = t.newState;
        return t.retVal;
      };
    };
  }
""" :: forall s b r. RefVal s -> (s -> {newState :: s, retVal :: b}) -> Eff (ref :: Ref | r) b

-- | Update the value of a mutable reference by applying a function
-- | to the current value.
modifyRef :: forall s r. RefVal s -> (s -> s) -> Eff (ref :: Ref | r) Unit
modifyRef ref f = modifyRef' ref (\s -> {newState: f s, retVal: unit})

-- | Update the value of a mutable reference to the specified value.
foreign import writeRef """
  function writeRef(ref) {
    return function(val) {
      return function() {
        ref.value = val;
        return {};
      };
    };
  }
""" :: forall s r. RefVal s -> s -> Eff (ref :: Ref | r) Unit
