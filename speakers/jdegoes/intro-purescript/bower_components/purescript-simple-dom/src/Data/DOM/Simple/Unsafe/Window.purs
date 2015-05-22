module Data.DOM.Simple.Unsafe.Window where

import DOM
import Control.Monad.Eff

import Data.DOM.Simple.Types

foreign import unsafeDocument
  """
  function unsafeDocument(win) {
    return function () {
      return win.document;
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) HTMLDocument)

foreign import unsafeNavigator
  """
  function unsafeNavigator(win) {
    return function () {
      return win.navigator;
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) DOMNavigator)

foreign import unsafeLocation
  """
  function unsafeLocation(win) {
    return function () {
      return win.location;
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) DOMLocation)

-- hmmm. useless re-cast?
foreign import unsafeGetLocation
  """
  function unsafeGetLocation(loc) {
    return function () {
      return loc;
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) String)

foreign import unsafeSetLocation
  """
  function unsafeSetLocation(value) {
    return function (loc) {
      return function () {
        location.assign(value);
      };
    };
  }""" :: forall eff a. String -> a -> (Eff (dom :: DOM | eff) Unit)

foreign import unsafeGetSearchLocation
  """
  function unsafeGetSearchLocation(loc) {
    return function () {
      return loc.search;
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) String)

foreign import unsafeSetTimeout
  """
  function unsafeSetTimeout(win) {
    return function(delay) {
      return function(func) {
        return function() {
          return win.setTimeout(func, delay);
        };
      };
    };
  }""" :: forall eff b. b -> Number -> Eff (dom :: DOM | eff) Unit -> (Eff (dom :: DOM | eff) Timeout)

foreign import unsafeSetInterval
  """
  function unsafeSetInterval(win) {
    return function(delay) {
      return function(func) {
        return function() {
          return win.setInterval(func, delay);
        };
      };
    };
  }""" :: forall eff b. b -> Number -> Eff (dom :: DOM | eff) Unit -> (Eff (dom :: DOM | eff) Timeout)

foreign import unsafeClearTimeout
  """
  function unsafeClearTimeout(win) {
    return function(timeout) {
      return function() {
        win.clearTimeout(timeout);
      };
    };
  }""" :: forall eff b. b -> Timeout -> (Eff (dom :: DOM | eff) Unit)

foreign import unsafeInnerWidth
  """
  function unsafeInnerWidth(win) {
    return function() {
      return win.innerWidth;
    };
  }""" :: forall eff b. b -> (Eff (dom :: DOM | eff) Number)

foreign import unsafeInnerHeight
  """
  function unsafeInnerHeight(win) {
    return function() {
      return win.innerHeight;
    };
  }""" :: forall eff b. b -> (Eff (dom :: DOM | eff) Number)
