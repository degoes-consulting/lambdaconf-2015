module Data.DOM.Simple.Unsafe.Utils where

import Data.Maybe

foreign import ensure3
  """
  function ensure3(nothing) {
    return function(just) {
      return function(v) {
        if (v === undefined || v === null) {
          return nothing;
        } else {
          return just(v);
        }
      };
   };
  }""" :: forall a. Maybe a -> (a -> Maybe a) -> a -> Maybe a

ensure = ensure3 Nothing Just

foreign import showImpl
  """
  function showImpl(v) {
    return function () {
      return v.toString();
    };
  }""" :: forall a. a -> String
