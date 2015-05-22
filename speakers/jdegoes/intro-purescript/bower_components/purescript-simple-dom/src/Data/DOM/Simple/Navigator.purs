module Data.DOM.Simple.Navigator where

import DOM
import Control.Monad.Eff

import Data.DOM.Simple.Unsafe.Navigator

import Data.DOM.Simple.Types
import Data.DOM.Simple.Element

class Navigator b where
  appName     :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  appVersion  :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  appCodeName :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  language    :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  platform    :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  product     :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  userAgent   :: forall eff. b -> (Eff (dom :: DOM | eff) String)

instance domNavigator :: Navigator DOMNavigator where
  appName = unsafeAppName
  appVersion = unsafeAppVersion
  appCodeName = unsafeAppCodeName
  language = unsafeLanguage
  platform = unsafePlatform
  product = unsafeProduct
  userAgent = unsafeUserAgent
