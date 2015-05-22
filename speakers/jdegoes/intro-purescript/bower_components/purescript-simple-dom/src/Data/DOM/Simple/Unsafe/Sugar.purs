module Data.DOM.Simple.Unsafe.Sugar where

import Control.Monad.Eff
import DOM

import Data.DOM.Simple.Element
import Data.DOM.Simple.Types

-- HACK: This is a really dirty hack, is there any way to unify
-- the kinds without having to do this? i'm at a loss.
foreign import dirtyKindDomRecast
  """
  function dirtyKindDomRecast(_) {
    return function id (x) {
        return x;
    };
  }""" :: forall eff effn a. (Element a) => (Eff eff a) -> (Eff (dom :: DOM | effn) a)
