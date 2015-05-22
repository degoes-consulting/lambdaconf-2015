module Data.DOM.Simple.Unsafe.Ajax where

import DOM
import Control.Monad.Eff
import Data.Function

import Data.DOM.Simple.Types

foreign import unsafeReadyState
  """
  function unsafeReadyState(obj) {
    return function () {
      return obj.readyState;
    };
  }""" :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) Number

foreign import unsafeOnReadyStateChange
  """
  function unsafeOnReadyStateChange(obj, fn) {
    return function () {
      obj.onreadystatechange = fn;
      return {};
    };
  }""" :: forall eff e. Fn2 XMLHttpRequest (Eff e Unit) (Eff (dom :: DOM | eff) Unit)

foreign import unsafeOpen
  """
  function unsafeOpen(obj, method, url) {
    return function () {
      obj.open(method, url);
      return {};
    };
  }""" :: forall eff. Fn3 XMLHttpRequest String String (Eff (dom :: DOM | eff) Unit)

foreign import unsafeSend
  """
  function unsafeSend(obj) {
    return function () {
      obj.send();
      return {};
    };
  }""" :: forall eff. Fn1 XMLHttpRequest (Eff (dom :: DOM | eff) Unit)

foreign import unsafeSendWithPayload
  """
  function unsafeSendWithPayload(obj, payload) {
    return function () {
      obj.send(payload);
      return {};
    };
  }""" :: forall eff a. Fn2 XMLHttpRequest a (Eff (dom :: DOM | eff) Unit)

foreign import unsafeSetResponseType
  """
  function unsafeSetResponseType(obj, rt) {
    return function () {
      obj.responseType = rt;
      return {};
    };
  }""" :: forall eff. Fn2 XMLHttpRequest String (Eff (dom :: DOM | eff) Unit)

foreign import unsafeResponseType
  """
  function unsafeResponseType(obj) {
    return function () {
      return obj.responseType;
    };
  }""" :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String

foreign import unsafeResponse
  """
  function unsafeResponse(obj) {
    return function () {
      return obj.response;
    };
  }""" :: forall eff a. XMLHttpRequest -> Eff (dom :: DOM | eff) a

foreign import unsafeGetResponseHeader
  """
  function unsafeGetResponseHeader(obj, key) {
    return function () {
      return obj.getResponseHeader(key);
    };
  }""" :: forall eff a. Fn2 XMLHttpRequest String (Eff (dom :: DOM | eff) String)
