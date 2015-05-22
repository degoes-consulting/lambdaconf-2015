module Data.DOM.Simple.Element where

import Control.Monad.Eff
import DOM

import Data.DOM.Simple.Unsafe.Utils(ensure, showImpl)
import Data.DOM.Simple.Unsafe.Element
import Data.DOM.Simple.Types

import Data.Foldable(for_)
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Tuple as T

class Element b where
  getElementById         :: forall eff. String -> b -> (Eff (dom :: DOM | eff) (Maybe HTMLElement))
  getElementsByClassName :: forall eff. String -> b -> (Eff (dom :: DOM | eff) [HTMLElement])
  getElementsByName      :: forall eff. String -> b -> (Eff (dom :: DOM | eff) [HTMLElement])
  querySelector          :: forall eff. String -> b -> (Eff (dom :: DOM | eff) (Maybe HTMLElement))
  querySelectorAll       :: forall eff. String -> b -> (Eff (dom :: DOM | eff) NodeList)
  getAttribute           :: forall eff. String -> b -> (Eff (dom :: DOM | eff) String)
  setAttribute           :: forall eff. String -> String -> b -> (Eff (dom :: DOM | eff) Unit)
  hasAttribute           :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Boolean)
  removeAttribute        :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Unit)
  getStyleAttr           :: forall eff. String -> b -> (Eff (dom :: DOM | eff) String)
  setStyleAttr           :: forall eff. String -> String -> b -> (Eff (dom :: DOM | eff) Unit)
  children               :: forall eff. b -> (Eff (dom :: DOM | eff) [HTMLElement])
  appendChild            :: forall eff. b -> HTMLElement -> (Eff (dom :: DOM | eff) Unit)
  innerHTML              :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  setInnerHTML           :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Unit)
  textContent            :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  setTextContent         :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Unit)
  value                  :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  setValue               :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Unit)
  contentWindow          :: forall eff. b -> (Eff (dom :: DOM | eff) HTMLWindow)
  classRemove            :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Unit)
  classAdd               :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Unit)
  classToggle            :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Unit)
  classContains          :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Boolean)

instance htmlElement :: Element HTMLElement where
  getElementById id el    = (unsafeGetElementById id el) >>= (ensure >>> return)
  getElementsByClassName  = unsafeGetElementsByClassName
  getElementsByName       = unsafeGetElementsByName
  querySelector sel el    = (unsafeQuerySelector sel el) >>= (ensure >>> return)
  querySelectorAll        = unsafeQuerySelectorAll
  getAttribute            = unsafeGetAttribute
  setAttribute            = unsafeSetAttribute
  hasAttribute            = unsafeHasAttribute
  removeAttribute         = unsafeRemoveAttribute
  getStyleAttr            = unsafeGetStyleAttr
  setStyleAttr            = unsafeSetStyleAttr
  children                = unsafeChildren
  appendChild             = unsafeAppendChild
  innerHTML               = unsafeInnerHTML
  setInnerHTML            = unsafeSetInnerHTML
  textContent             = unsafeTextContent
  setTextContent          = unsafeSetTextContent
  value                   = unsafeValue
  setValue                = unsafeSetValue
  contentWindow           = unsafeContentWindow
  classRemove             = unsafeClassRemove
  classAdd                = unsafeClassAdd
  classToggle             = unsafeClassToggle
  classContains           = unsafeClassContains

instance showHtmlElement :: Show HTMLElement where
  show = showImpl

setAttributes :: forall eff a. (Element a) => [(T.Tuple String String)] -> a -> (Eff (dom :: DOM | eff) Unit)
setAttributes xs el = for_ xs (\kv -> setAttribute (T.fst kv) (T.snd kv) el)

setStyleAttrs :: forall eff a. (Element a) => [(T.Tuple String String)] -> a -> (Eff (dom :: DOM | eff) Unit)
setStyleAttrs xs el = for_ xs (\kv -> setStyleAttr (T.fst kv) (T.snd kv) el)

click :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
click = unsafeClick

focus :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
focus = unsafeFocus

blur :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
blur = unsafeBlur
