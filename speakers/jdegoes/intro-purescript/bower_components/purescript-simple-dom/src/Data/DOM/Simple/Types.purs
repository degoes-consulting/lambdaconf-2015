module Data.DOM.Simple.Types where

import Control.Monad.Eff

foreign import data HTMLElement       :: *
foreign import data HTMLDocument      :: *
foreign import data HTMLWindow        :: *
foreign import data XMLHttpRequest    :: *
foreign import data DOMNavigator      :: *
foreign import data DOMEvent          :: *
foreign import data DOMLocation       :: *
foreign import data JavascriptContext :: *
foreign import data Timeout           :: *
