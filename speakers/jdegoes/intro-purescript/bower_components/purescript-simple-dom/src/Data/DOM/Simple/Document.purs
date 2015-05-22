module Data.DOM.Simple.Document where

import DOM
import Control.Monad.Eff

import Data.DOM.Simple.Types
import Data.DOM.Simple.Element
import Data.DOM.Simple.Unsafe.Utils(ensure, showImpl)
import Data.DOM.Simple.Unsafe.Element
import Data.DOM.Simple.Unsafe.Document

class Document b where
  title     :: forall eff. b -> (Eff (dom :: DOM | eff) String)
  setTitle  :: forall eff. String -> b -> (Eff (dom :: DOM | eff) Unit)
  body      :: forall eff. b -> (Eff (dom :: DOM | eff) HTMLElement)
  setBody   :: forall eff. HTMLElement -> b -> (Eff (dom :: DOM | eff) Unit)

instance htmlDocumentElement :: Element HTMLDocument where
  getElementById id el    = (unsafeGetElementById id el) >>= (return <<< ensure)
  getElementsByClassName  = unsafeGetElementsByClassName
  getElementsByName       = unsafeGetElementsByName
  querySelector sel el    = (unsafeQuerySelector sel el) >>= (return <<< ensure)
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

instance htmlDocument :: Document HTMLDocument where
  title                   = unsafeTitle
  setTitle                = unsafeSetTitle
  body                    = unsafeBody
  setBody                 = unsafeSetBody

instance showHtmlDocument :: Show HTMLDocument where
  show = showImpl
