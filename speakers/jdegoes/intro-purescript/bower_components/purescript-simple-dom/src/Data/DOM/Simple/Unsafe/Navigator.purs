module Data.DOM.Simple.Unsafe.Navigator where

import DOM
import Control.Monad.Eff

import Data.DOM.Simple.Types

foreign import unsafeAppName
  """
  function unsafeAppName(navi) {
    return function () {
      return navi.appName;
    };
  }""" ::forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String

foreign import unsafeAppVersion
  """
  function unsafeAppVersion(navi) {
    return function () {
      return navi.appName;
    };
  }""" ::forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String

foreign import unsafeAppCodeName
  """
  function unsafeAppCodeName(navi) {
    return function () {
      return navi.appCodeName;
    };
  }""" ::forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String

foreign import unsafeLanguage
  """
  function unsafeLanguage(navi) {
    return function () {
      return navi.language;
    };
  }""" ::forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String

foreign import unsafePlatform
  """
  function unsafePlatform(navi) {
    return function () {
      return navi.platform;
    };
  }""" ::forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String

foreign import unsafeProduct
  """
  function unsafeProduct(navi) {
    return function () {
      return navi.product;
    };
  }""" ::forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String

foreign import unsafeUserAgent
  """
  function unsafeUserAgent(navi) {
    return function () {
      return navi.userAgent;
    };
  }""" ::forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String
