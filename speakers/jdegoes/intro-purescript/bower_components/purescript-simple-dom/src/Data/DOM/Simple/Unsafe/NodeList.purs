module Data.DOM.Simple.Unsafe.NodeList where

import DOM
import Control.Monad.Eff

import Data.DOM.Simple.Types

foreign import unsafeNodeListLength
"""
function unsafeNodeListLength(nl) {
  return function () {
    return nl.length;
  }
}""" :: forall eff. NodeList -> Eff (dom :: DOM | eff) Number

foreign import unsafeNodeListItem
"""
function unsafeNodeListItem(idx) {
  return function (nl) {
    return function () {
      return nl.item(idx);
    }
  }
}""" :: forall eff. Number -> NodeList -> Eff (dom :: DOM | eff) HTMLElement

foreign import unsafeNodeListToArray
"""
function unsafeNodeListToArray(nl) {
  return function () {
    return Array.prototype.slice.call(nl);
  };
}""" ::forall eff. NodeList -> Eff (dom :: DOM | eff) [HTMLElement]
