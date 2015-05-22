module Data.Int.Bits
  ( (.&.)
  , (.|.)
  , (.^.)
  , shl
  , shr
  , zshr
  , complement
  ) where

import Prelude ()
import Data.Int

infixl 10 .&.
infixl 10 .|.
infixl 10 .^.

(.&.) :: Int -> Int -> Int
(.&.) = andImpl

(.|.) :: Int -> Int -> Int
(.|.) = orImpl

(.^.) :: Int -> Int -> Int
(.^.) = xorImpl

foreign import andImpl
  """
  function andImpl(n1) {
    return function(n2) {
      return n1 & n2;
    };
  }
  """ :: Int -> Int -> Int

foreign import orImpl
  """
  function orImpl(n1) {
    return function(n2) {
      return n1 | n2;
    };
  }
  """ :: Int -> Int -> Int

foreign import xorImpl
  """
  function xorImpl(n1) {
    return function(n2) {
      return n1 ^ n2;
    };
  }
  """ :: Int -> Int -> Int

foreign import shl
  """
  function shl(n1) {
    return function(n2) {
      return n1 << n2;
    };
  }
  """ :: Int -> Int -> Int

foreign import shr
  """
  function shr(n1) {
    return function(n2) {
      return n1 >> n2;
    };
  }
  """ :: Int -> Int -> Int

foreign import zshr
  """
  function zshr(n1) {
    return function(n2) {
      return n1 >>> n2;
    };
  }
  """ :: Int -> Int -> Int

foreign import complement
  """
  function complement(n) {
    return ~n;
  }
  """ :: Int -> Int
