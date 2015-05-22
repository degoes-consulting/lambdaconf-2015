module Data.DOM.Simple.Encode where

import DOM
import Control.Monad.Eff
import Data.DOM.Simple.Types

foreign import encodeURIComponent :: String -> String
foreign import decodeURIComponent :: String -> String
foreign import encodeURI :: String -> String
foreign import decodeURI :: String -> String

-- | Given an object, convert it into URL parameters.
foreign import paramatize
  """
  function paramatize(obj) {
    return Object.keys(obj).map(function(key) {
      return key + '=' + encodeURIComponent(obj[key]);
    }).join('&');
  }""" :: forall a. a -> String

-- | Given an object, convert it into a JSON string
foreign import toJsonString
  """
  function toJsonString(obj) {
    return function () {
      return JSON.stringify(obj);
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) String)
