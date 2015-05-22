-- | This module defines the `Error` type class, which is used with the error monad
-- | transformer, `ErrorT`.

module Control.Monad.Error where

-- | The `Error` type class represents _error_ types, which can be 
-- | constructed from error message strings.
class Error a where
  noMsg :: a
  strMsg :: String -> a

instance errorString :: Error String where
  noMsg = ""
  strMsg = id

