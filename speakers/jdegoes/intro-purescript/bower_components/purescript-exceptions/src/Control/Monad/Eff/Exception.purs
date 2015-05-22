-- | This module defines an effect, actions and handlers for working
-- | with Javascript exceptions.

module Control.Monad.Eff.Exception 
  ( Exception()
  , Error()
  , error
  , message
  , throwException
  , catchException
  ) where

import Control.Monad.Eff

-- | This effect is used to annotate code which possibly throws exceptions
foreign import data Exception :: !

-- | The type of Javascript errors
foreign import data Error :: *

instance showError :: Show Error where
  show = showErrorImpl

foreign import showErrorImpl
  """
  function showErrorImpl(err) {
    return err.stack || err.toString();
  }
  """ :: Error -> String

-- | Create a Javascript error, specifying a message
foreign import error
  """
  function error(msg) {
    return new Error(msg);
  }
  """ :: String -> Error

-- | Get the error message from a Javascript error
foreign import message
  """
  function message(e) {
    return e.message;
  }
  """ :: Error -> String

-- | Throw an exception
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = do
-- |   x <- readNumber
-- |   when (x < 0) $ throwException $ 
-- |     error "Expected a non-negative number"
-- | ```
foreign import throwException
  """
  function throwException(e) {
    return function() {
      throw e;
    };
  }
  """ :: forall a eff. Error -> Eff (err :: Exception | eff) a

-- | Catch an exception by providing an exception handler.
-- |
-- | This handler removes the `Exception` effect.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = catchException print do
-- |   trace "Exceptions thrown in this block will be logged to the console"
-- | ```
foreign import catchException
  """
  function catchException(c) {
    return function(t) {
      return function() {
        try {
          return t();
        } catch(e) {
          if (e instanceof Error || Object.prototype.toString.call(e) === '[object Error]') {
            return c(e)();
          } else {
            return c(new Error(e.toString()))();
          }
        }
      };
    };
  }
  """ :: forall a eff. (Error -> Eff eff a) -> Eff (err :: Exception | eff) a -> Eff eff a
