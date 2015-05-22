module Data.DOM.Simple.Unsafe.Events where

import DOM
import Control.Monad.Eff

import Data.DOM.Simple.Types

foreign import unsafeAddEventListener
  """
  function unsafeAddEventListener(targ) {
    return function (cb) {
       return function (src) {
         return function () {
           src.addEventListener(targ, function(evt) {
             cb(evt)();
           });
         };
       };
    };
  }""" :: forall eff t e b. String -> (e -> Eff (dom :: DOM | t) Unit) -> b -> (Eff (dom :: DOM | eff) Unit)

foreign import unsafeRemoveEventListener
  """
  function unsafeRemoveEventListener(targ) {
    return function (cb) {
       return function (src) {
         return function () {
           src.removeEventListener(targ, function (evt) {
             cb(evt)();
           });
         };
       };
    };
  }""" :: forall eff t e b. String -> (e -> Eff (dom :: DOM | t) Unit) -> b -> (Eff (dom :: DOM | eff) Unit)

foreign import unsafeEventTarget
  """
  function unsafeEventTarget(event) {
    return function () {
      return event.target;
    };
  }""" :: forall eff a. DOMEvent -> (Eff (dom :: DOM | eff) a)

foreign import unsafeStopPropagation
  """
  function unsafeStopPropagation(event) {
    return function () {
      event.stopPropagation();
    };
  }""" :: forall eff. DOMEvent -> (Eff (dom :: DOM | eff) Unit)

foreign import unsafePreventDefault
  """
  function unsafePreventDefault(event) {
    return function () {
      event.preventDefault();
    };
  }""" :: forall eff. DOMEvent -> (Eff (dom :: DOM | eff) Unit)

-- XXX Wallpaper over the fact that some browsers don't support
-- KeyboardEvent.key yet.  It's a hack, since it doesn't correctly
-- handle modifier keys, etc.
foreign import unsafeEventKey
  """
  function unsafeEventKey(event) {
    return function() {
      return event.key === undefined
         ? String.fromCharCode(event.keyCode)
         : event.key;
    };
  }""" :: forall eff. DOMEvent -> (Eff (dom :: DOM | eff) String)

foreign import unsafeEventKeyCode
  """
  function unsafeEventKeyCode(event) {
    return function() {
      return event.keyCode;
    };
  }""" :: forall eff. DOMEvent -> (Eff (dom :: DOM | eff) Number)

foreign import unsafeEventNumberProp
  """
  function unsafeEventNumberProp(prop) {
    return function (event) {
      return function() {
        return event[prop];
      };
    };
  }""" :: forall eff. String -> DOMEvent -> (Eff (dom :: DOM | eff) Number)

foreign import unsafeEventStringProp
  """
  function unsafeEventStringProp(prop) {
    return function (event) {
      return function() {
        return event[prop];
      };
    };
  }""" :: forall eff. String -> DOMEvent -> (Eff (dom :: DOM | eff) String)

foreign import unsafeEventBooleanProp
  """
  function unsafeEventBooleanProp(prop) {
    return function (event) {
      return function() {
        return !!event[prop];
      };
    };
  }""" :: forall eff. String -> DOMEvent -> (Eff (dom :: DOM | eff) Boolean)

-- XXX really should be returning an HTMLAbstractView here...
foreign import unsafeEventView
  """
  function unsafeEventView(event) {
    return function() {
      return event.view;
    };
  }""" :: forall eff. DOMEvent -> (Eff (dom :: DOM | eff) HTMLWindow)
