module Data.DOM.Simple.Unsafe.Document where

import DOM
import Control.Monad.Eff

import Data.DOM.Simple.Types

foreign import unsafeTitle
  """
  function unsafeTitle(src) {
    return function () {
      return src.title;
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) String)

foreign import unsafeSetTitle
  """
  function unsafeSetTitle(value) {
    return function (src) {
      return function () {
        src.title = value;
      };
    };
  }""" :: forall eff a. String -> a -> (Eff (dom :: DOM | eff) Unit)

foreign import unsafeBody
  """
  function unsafeBody(src) {
    return function () {
      return src.body;
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) HTMLElement)

foreign import unsafeSetBody
  """
  function unsafeSetBody(value) {
    return function (src) {
      return function () {
        src.body = value;
      };
    };
  }""" :: forall eff a. HTMLElement -> a -> (Eff (dom :: DOM | eff) Unit)
