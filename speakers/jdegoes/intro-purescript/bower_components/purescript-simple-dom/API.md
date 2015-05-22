# Module Documentation

## Module Data.DOM.Simple.Ajax

#### `ReadyState`

``` purescript
data ReadyState
  = Unsent 
  | Opened 
  | HeadersReceived 
  | Loading 
  | Done 
```


#### `Url`

``` purescript
type Url = String
```


#### `HttpMethod`

``` purescript
data HttpMethod
  = GET 
  | POST 
  | PUT 
  | DELETE 
  | PATCH 
  | HEAD 
  | OPTIONS 
  | JSONP 
  | HttpMethod String
```


#### `ResponseType`

``` purescript
data ResponseType
  = Default 
  | ArrayBuffer 
  | Blob 
  | Document 
  | Json 
  | Text 
  | MozBlob 
  | MozChunkedText 
  | MozChunkedArrayBuffer 
```


#### `ArrayBuffer`

``` purescript
data ArrayBuffer :: *
```

#### `ArrayBufferView`

``` purescript
data ArrayBufferView :: *
```


#### `Blob`

``` purescript
data Blob :: *
```


#### `FormData`

``` purescript
data FormData :: *
```


#### `HttpData`

``` purescript
data HttpData a
  = NoData 
  | TextData String
  | ArrayBufferData ArrayBuffer
  | ArrayBufferViewData ArrayBufferView
  | BlobData Blob
  | FormData FormData
  | DocumentData HTMLDocument
  | JsonData a
```


#### `showHttpMethod`

``` purescript
instance showHttpMethod :: Show HttpMethod
```


#### `showResponseType`

``` purescript
instance showResponseType :: Show ResponseType
```


#### `makeXMLHttpRequest`

``` purescript
makeXMLHttpRequest :: forall eff. Eff (dom :: DOM | eff) XMLHttpRequest
```


#### `readyState`

``` purescript
readyState :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) ReadyState
```


#### `onReadyStateChange`

``` purescript
onReadyStateChange :: forall eff e. Eff e Unit -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
```


#### `open`

``` purescript
open :: forall eff. HttpMethod -> Url -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
```


#### `send`

``` purescript
send :: forall eff a. HttpData a -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
```


#### `setResponseType`

``` purescript
setResponseType :: forall eff. ResponseType -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
```

#### `responseType`

``` purescript
responseType :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) ResponseType
```


#### `response`

``` purescript
response :: forall eff a. XMLHttpRequest -> Eff (dom :: DOM | eff) (HttpData a)
```


#### `responseText`

``` purescript
responseText :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String
```


#### `status`

``` purescript
status :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) Number
```


#### `statusText`

``` purescript
statusText :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String
```


#### `setRequestHeader`

``` purescript
setRequestHeader :: forall eff. String -> String -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
```


#### `getAllResponseHeaders`

``` purescript
getAllResponseHeaders :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String
```


#### `getResponseHeader`

``` purescript
getResponseHeader :: forall eff. String -> XMLHttpRequest -> Eff (dom :: DOM | eff) (Maybe String)
```


#### `overrideMimeType`

``` purescript
overrideMimeType :: forall eff. String -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
```



## Module Data.DOM.Simple.Document

#### `Document`

``` purescript
class Document b where
  title :: forall eff. b -> Eff (dom :: DOM | eff) String
  setTitle :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  body :: forall eff. b -> Eff (dom :: DOM | eff) HTMLElement
  setBody :: forall eff. HTMLElement -> b -> Eff (dom :: DOM | eff) Unit
```


#### `htmlDocumentElement`

``` purescript
instance htmlDocumentElement :: Element HTMLDocument
```


#### `htmlDocument`

``` purescript
instance htmlDocument :: Document HTMLDocument
```


#### `showHtmlDocument`

``` purescript
instance showHtmlDocument :: Show HTMLDocument
```



## Module Data.DOM.Simple.Element

#### `Element`

``` purescript
class Element b where
  getElementById :: forall eff. String -> b -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
  getElementsByClassName :: forall eff. String -> b -> Eff (dom :: DOM | eff) [HTMLElement]
  getElementsByName :: forall eff. String -> b -> Eff (dom :: DOM | eff) [HTMLElement]
  querySelector :: forall eff. String -> b -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
  querySelectorAll :: forall eff. String -> b -> Eff (dom :: DOM | eff) NodeList
  getAttribute :: forall eff. String -> b -> Eff (dom :: DOM | eff) String
  setAttribute :: forall eff. String -> String -> b -> Eff (dom :: DOM | eff) Unit
  hasAttribute :: forall eff. String -> b -> Eff (dom :: DOM | eff) Boolean
  removeAttribute :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  getStyleAttr :: forall eff. String -> b -> Eff (dom :: DOM | eff) String
  setStyleAttr :: forall eff. String -> String -> b -> Eff (dom :: DOM | eff) Unit
  children :: forall eff. b -> Eff (dom :: DOM | eff) [HTMLElement]
  appendChild :: forall eff. b -> HTMLElement -> Eff (dom :: DOM | eff) Unit
  innerHTML :: forall eff. b -> Eff (dom :: DOM | eff) String
  setInnerHTML :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  textContent :: forall eff. b -> Eff (dom :: DOM | eff) String
  setTextContent :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  value :: forall eff. b -> Eff (dom :: DOM | eff) String
  setValue :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  contentWindow :: forall eff. b -> Eff (dom :: DOM | eff) HTMLWindow
  classRemove :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  classAdd :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  classToggle :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  classContains :: forall eff. String -> b -> Eff (dom :: DOM | eff) Boolean
```


#### `htmlElement`

``` purescript
instance htmlElement :: Element HTMLElement
```


#### `showHtmlElement`

``` purescript
instance showHtmlElement :: Show HTMLElement
```


#### `setAttributes`

``` purescript
setAttributes :: forall eff a. (Element a) => [T.Tuple String String] -> a -> Eff (dom :: DOM | eff) Unit
```


#### `setStyleAttrs`

``` purescript
setStyleAttrs :: forall eff a. (Element a) => [T.Tuple String String] -> a -> Eff (dom :: DOM | eff) Unit
```


#### `click`

``` purescript
click :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
```


#### `focus`

``` purescript
focus :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
```


#### `blur`

``` purescript
blur :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
```



## Module Data.DOM.Simple.Encode

#### `encodeURIComponent`

``` purescript
encodeURIComponent :: String -> String
```


#### `decodeURIComponent`

``` purescript
decodeURIComponent :: String -> String
```


#### `encodeURI`

``` purescript
encodeURI :: String -> String
```


#### `decodeURI`

``` purescript
decodeURI :: String -> String
```


#### `paramatize`

``` purescript
paramatize :: forall a. a -> String
```

Given an object, convert it into URL parameters.

#### `toJsonString`

``` purescript
toJsonString :: forall eff a. a -> Eff (dom :: DOM | eff) String
```

Given an object, convert it into a JSON string


## Module Data.DOM.Simple.Events

#### `Read`

``` purescript
class Read s where
  read :: String -> s
```

#### `Event`

``` purescript
class Event e where
  eventTarget :: forall eff a. e -> Eff (dom :: DOM | eff) a
  stopPropagation :: forall eff. e -> Eff (dom :: DOM | eff) Unit
  preventDefault :: forall eff. e -> Eff (dom :: DOM | eff) Unit
```

#### `eventDOMEvent`

``` purescript
instance eventDOMEvent :: Event DOMEvent
```


#### `MouseEventType`

``` purescript
data MouseEventType
  = MouseMoveEvent 
  | MouseOverEvent 
  | MouseEnterEvent 
  | MouseOutEvent 
  | MouseLeaveEvent 
```

#### `mouseEventTypeShow`

``` purescript
instance mouseEventTypeShow :: Show MouseEventType
```


#### `mouseEventTypeRead`

``` purescript
instance mouseEventTypeRead :: Read MouseEventType
```


#### `MouseEvent`

``` purescript
class (Event e) <= MouseEvent e where
  mouseEventType :: forall eff. e -> Eff (dom :: DOM | eff) MouseEventType
  screenX :: forall eff. e -> Eff (dom :: DOM | eff) Number
  screenY :: forall eff. e -> Eff (dom :: DOM | eff) Number
```


#### `mouseEventDOMEvent`

``` purescript
instance mouseEventDOMEvent :: MouseEvent DOMEvent
```


#### `MouseEventTarget`

``` purescript
class MouseEventTarget b where
  addMouseEventListener :: forall e t ta. (MouseEvent e) => MouseEventType -> (e -> Eff (dom :: DOM | t) Unit) -> b -> Eff (dom :: DOM | ta) Unit
  removeMouseEventListener :: forall e t ta. (MouseEvent e) => MouseEventType -> (e -> Eff (dom :: DOM | t) Unit) -> b -> Eff (dom :: DOM | ta) Unit
```


#### `mouseEventTargetHTMLWindow`

``` purescript
instance mouseEventTargetHTMLWindow :: MouseEventTarget HTMLWindow
```


#### `mouseEventTargetHTMLDocument`

``` purescript
instance mouseEventTargetHTMLDocument :: MouseEventTarget HTMLDocument
```


#### `mouseEventTargetHTMLElement`

``` purescript
instance mouseEventTargetHTMLElement :: MouseEventTarget HTMLElement
```


#### `KeyboardEventType`

``` purescript
data KeyboardEventType
  = KeydownEvent 
  | KeypressEvent 
  | KeyupEvent 
```

#### `keyboardEventTypeShow`

``` purescript
instance keyboardEventTypeShow :: Show KeyboardEventType
```


#### `keyboardEventTypeRead`

``` purescript
instance keyboardEventTypeRead :: Read KeyboardEventType
```


#### `KeyLocation`

``` purescript
data KeyLocation
  = KeyLocationStandard 
  | KeyLocationLeft 
  | KeyLocationRight 
  | KeyLocationNumpad 
```


#### `toKeyLocation`

``` purescript
toKeyLocation :: Number -> KeyLocation
```


#### `KeyboardEvent`

``` purescript
class (Event e) <= KeyboardEvent e where
  keyboardEventType :: forall eff. e -> Eff (dom :: DOM | eff) KeyboardEventType
  key :: forall eff. e -> Eff (dom :: DOM | eff) String
  keyCode :: forall eff. e -> Eff (dom :: DOM | eff) Number
  keyLocation :: forall eff. e -> Eff (dom :: DOM | eff) KeyLocation
  altKey :: forall eff. e -> Eff (dom :: DOM | eff) Boolean
  ctrlKey :: forall eff. e -> Eff (dom :: DOM | eff) Boolean
  metaKey :: forall eff. e -> Eff (dom :: DOM | eff) Boolean
  shiftKey :: forall eff. e -> Eff (dom :: DOM | eff) Boolean
```


#### `keyboardEventDOMEvent`

``` purescript
instance keyboardEventDOMEvent :: KeyboardEvent DOMEvent
```


#### `KeyboardEventTarget`

``` purescript
class KeyboardEventTarget b where
  addKeyboardEventListener :: forall e t ta. (KeyboardEvent e) => KeyboardEventType -> (e -> Eff (dom :: DOM | t) Unit) -> b -> Eff (dom :: DOM | ta) Unit
  removeKeyboardEventListener :: forall e t ta. (KeyboardEvent e) => KeyboardEventType -> (e -> Eff (dom :: DOM | t) Unit) -> b -> Eff (dom :: DOM | ta) Unit
```


#### `keyboardEventTargetHTMLWindow`

``` purescript
instance keyboardEventTargetHTMLWindow :: KeyboardEventTarget HTMLWindow
```


#### `keyboardEventTargetHTMLDocument`

``` purescript
instance keyboardEventTargetHTMLDocument :: KeyboardEventTarget HTMLDocument
```


#### `keyboardEventTargetHTMLElement`

``` purescript
instance keyboardEventTargetHTMLElement :: KeyboardEventTarget HTMLElement
```


#### `UIEventType`

``` purescript
data UIEventType
  = LoadEvent 
  | UnloadEvent 
  | AbortEvent 
  | ErrorEvent 
  | SelectEvent 
  | ResizeEvent 
  | ScrollEvent 
```

#### `uiEventTypeShow`

``` purescript
instance uiEventTypeShow :: Show UIEventType
```


#### `uiEventTypeRead`

``` purescript
instance uiEventTypeRead :: Read UIEventType
```


#### `UIEvent`

``` purescript
class (Event e) <= UIEvent e where
  view :: forall eff. e -> Eff (dom :: DOM | eff) HTMLWindow
  detail :: forall eff. e -> Eff (dom :: DOM | eff) Number
```


#### `uiEventDOMEvent`

``` purescript
instance uiEventDOMEvent :: UIEvent DOMEvent
```


#### `UIEventTarget`

``` purescript
class UIEventTarget b where
  addUIEventListener :: forall e t ta. (UIEvent e) => UIEventType -> (e -> Eff (dom :: DOM | t) Unit) -> b -> Eff (dom :: DOM | ta) Unit
  removeUIEventListener :: forall e t ta. (UIEvent e) => UIEventType -> (e -> Eff (dom :: DOM | t) Unit) -> b -> Eff (dom :: DOM | ta) Unit
```


#### `uiEventTargetHTMLWindow`

``` purescript
instance uiEventTargetHTMLWindow :: UIEventTarget HTMLWindow
```



## Module Data.DOM.Simple.Navigator

#### `Navigator`

``` purescript
class Navigator b where
  appName :: forall eff. b -> Eff (dom :: DOM | eff) String
  appVersion :: forall eff. b -> Eff (dom :: DOM | eff) String
  appCodeName :: forall eff. b -> Eff (dom :: DOM | eff) String
  language :: forall eff. b -> Eff (dom :: DOM | eff) String
  platform :: forall eff. b -> Eff (dom :: DOM | eff) String
  product :: forall eff. b -> Eff (dom :: DOM | eff) String
  userAgent :: forall eff. b -> Eff (dom :: DOM | eff) String
```


#### `domNavigator`

``` purescript
instance domNavigator :: Navigator DOMNavigator
```



## Module Data.DOM.Simple.NodeList

#### `NodeListInst`

``` purescript
class NodeListInst b where
  length :: forall eff. b -> Eff (dom :: DOM | eff) Number
  item :: forall eff. Number -> b -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
```


#### `nodeList`

``` purescript
instance nodeList :: NodeListInst NodeList
```


#### `nodeListToArray`

``` purescript
nodeListToArray :: forall eff. NodeList -> Eff (dom :: DOM | eff) [HTMLElement]
```


#### `nodeListToArray'`

``` purescript
nodeListToArray' :: forall eff. NodeList -> Eff (dom :: DOM | eff) [HTMLElement]
```



## Module Data.DOM.Simple.Sugar

#### `DOMArrows`

``` purescript
class DOMArrows b where
  (#<-) :: forall eff. b -> Tuple String String -> Eff (dom :: DOM | eff) Unit
  (<-#) :: forall eff. b -> String -> Eff (dom :: DOM | eff) String
  (<-?) :: forall eff. b -> String -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
  (%<-) :: forall eff. b -> String -> Eff (dom :: DOM | eff) Unit
  (@<-) :: forall eff. b -> String -> Eff (dom :: DOM | eff) Unit
```


#### `arrowsHTMLElement`

``` purescript
instance arrowsHTMLElement :: (Element a) => DOMArrows a
```

#### `arrowsEffHTMLElement`

``` purescript
instance arrowsEffHTMLElement :: (Element a) => DOMArrows (Eff eff a)
```

#### `arrowsMaybeHTMLElement`

``` purescript
instance arrowsMaybeHTMLElement :: (Element a) => DOMArrows (Maybe a)
```


## Module Data.DOM.Simple.Types

#### `HTMLElement`

``` purescript
data HTMLElement :: *
```


#### `HTMLDocument`

``` purescript
data HTMLDocument :: *
```


#### `HTMLWindow`

``` purescript
data HTMLWindow :: *
```


#### `XMLHttpRequest`

``` purescript
data XMLHttpRequest :: *
```


#### `DOMNavigator`

``` purescript
data DOMNavigator :: *
```


#### `DOMEvent`

``` purescript
data DOMEvent :: *
```


#### `DOMLocation`

``` purescript
data DOMLocation :: *
```


#### `JavascriptContext`

``` purescript
data JavascriptContext :: *
```


#### `Timeout`

``` purescript
data Timeout :: *
```



## Module Data.DOM.Simple.Unsafe.Ajax

#### `unsafeReadyState`

``` purescript
unsafeReadyState :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) Number
```


#### `unsafeOnReadyStateChange`

``` purescript
unsafeOnReadyStateChange :: forall eff e. Fn2 XMLHttpRequest (Eff e Unit) (Eff (dom :: DOM | eff) Unit)
```


#### `unsafeOpen`

``` purescript
unsafeOpen :: forall eff. Fn3 XMLHttpRequest String String (Eff (dom :: DOM | eff) Unit)
```


#### `unsafeSend`

``` purescript
unsafeSend :: forall eff. Fn1 XMLHttpRequest (Eff (dom :: DOM | eff) Unit)
```


#### `unsafeSendWithPayload`

``` purescript
unsafeSendWithPayload :: forall eff a. Fn2 XMLHttpRequest a (Eff (dom :: DOM | eff) Unit)
```


#### `unsafeSetResponseType`

``` purescript
unsafeSetResponseType :: forall eff. Fn2 XMLHttpRequest String (Eff (dom :: DOM | eff) Unit)
```


#### `unsafeResponseType`

``` purescript
unsafeResponseType :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String
```


#### `unsafeResponse`

``` purescript
unsafeResponse :: forall eff a. XMLHttpRequest -> Eff (dom :: DOM | eff) a
```


#### `unsafeGetResponseHeader`

``` purescript
unsafeGetResponseHeader :: forall eff a. Fn2 XMLHttpRequest String (Eff (dom :: DOM | eff) String)
```



## Module Data.DOM.Simple.Unsafe.Document

#### `unsafeTitle`

``` purescript
unsafeTitle :: forall eff a. a -> Eff (dom :: DOM | eff) String
```


#### `unsafeSetTitle`

``` purescript
unsafeSetTitle :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeBody`

``` purescript
unsafeBody :: forall eff a. a -> Eff (dom :: DOM | eff) HTMLElement
```


#### `unsafeSetBody`

``` purescript
unsafeSetBody :: forall eff a. HTMLElement -> a -> Eff (dom :: DOM | eff) Unit
```



## Module Data.DOM.Simple.Unsafe.Element

#### `unsafeGetElementById`

``` purescript
unsafeGetElementById :: forall eff a. String -> a -> Eff (dom :: DOM | eff) HTMLElement
```


#### `unsafeGetElementsByClassName`

``` purescript
unsafeGetElementsByClassName :: forall eff a. String -> a -> Eff (dom :: DOM | eff) [HTMLElement]
```


#### `unsafeGetElementsByName`

``` purescript
unsafeGetElementsByName :: forall eff a. String -> a -> Eff (dom :: DOM | eff) [HTMLElement]
```


#### `unsafeQuerySelector`

``` purescript
unsafeQuerySelector :: forall eff a. String -> a -> Eff (dom :: DOM | eff) HTMLElement
```


#### `unsafeQuerySelectorAll`

``` purescript
unsafeQuerySelectorAll :: forall eff a. String -> a -> Eff (dom :: DOM | eff) NodeList
```


#### `unsafeGetAttribute`

``` purescript
unsafeGetAttribute :: forall eff a. String -> a -> Eff (dom :: DOM | eff) String
```


#### `unsafeSetAttribute`

``` purescript
unsafeSetAttribute :: forall eff a. String -> String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeHasAttribute`

``` purescript
unsafeHasAttribute :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Boolean
```


#### `unsafeRemoveAttribute`

``` purescript
unsafeRemoveAttribute :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeGetStyleAttr`

``` purescript
unsafeGetStyleAttr :: forall eff a. String -> a -> Eff (dom :: DOM | eff) String
```


#### `unsafeSetStyleAttr`

``` purescript
unsafeSetStyleAttr :: forall eff a. String -> String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeChildren`

``` purescript
unsafeChildren :: forall eff a. a -> Eff (dom :: DOM | eff) [HTMLElement]
```


#### `unsafeAppendChild`

``` purescript
unsafeAppendChild :: forall eff a. a -> HTMLElement -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeInnerHTML`

``` purescript
unsafeInnerHTML :: forall eff a. a -> Eff (dom :: DOM | eff) String
```


#### `unsafeSetInnerHTML`

``` purescript
unsafeSetInnerHTML :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeTextContent`

``` purescript
unsafeTextContent :: forall eff a. a -> Eff (dom :: DOM | eff) String
```


#### `unsafeSetTextContent`

``` purescript
unsafeSetTextContent :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeValue`

``` purescript
unsafeValue :: forall eff a. a -> Eff (dom :: DOM | eff) String
```


#### `unsafeSetValue`

``` purescript
unsafeSetValue :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeContentWindow`

``` purescript
unsafeContentWindow :: forall eff a. a -> Eff (dom :: DOM | eff) HTMLWindow
```


#### `unsafeClassAdd`

``` purescript
unsafeClassAdd :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeClassRemove`

``` purescript
unsafeClassRemove :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeClassToggle`

``` purescript
unsafeClassToggle :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeClassContains`

``` purescript
unsafeClassContains :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Boolean
```


#### `unsafeClick`

``` purescript
unsafeClick :: forall eff a. a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeFocus`

``` purescript
unsafeFocus :: forall eff a. a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeBlur`

``` purescript
unsafeBlur :: forall eff a. a -> Eff (dom :: DOM | eff) Unit
```



## Module Data.DOM.Simple.Unsafe.Events

#### `unsafeAddEventListener`

``` purescript
unsafeAddEventListener :: forall eff t e b. String -> (e -> Eff (dom :: DOM | t) Unit) -> b -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeRemoveEventListener`

``` purescript
unsafeRemoveEventListener :: forall eff t e b. String -> (e -> Eff (dom :: DOM | t) Unit) -> b -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeEventTarget`

``` purescript
unsafeEventTarget :: forall eff a. DOMEvent -> Eff (dom :: DOM | eff) a
```


#### `unsafeStopPropagation`

``` purescript
unsafeStopPropagation :: forall eff. DOMEvent -> Eff (dom :: DOM | eff) Unit
```


#### `unsafePreventDefault`

``` purescript
unsafePreventDefault :: forall eff. DOMEvent -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeEventKey`

``` purescript
unsafeEventKey :: forall eff. DOMEvent -> Eff (dom :: DOM | eff) String
```

#### `unsafeEventKeyCode`

``` purescript
unsafeEventKeyCode :: forall eff. DOMEvent -> Eff (dom :: DOM | eff) Number
```


#### `unsafeEventNumberProp`

``` purescript
unsafeEventNumberProp :: forall eff. String -> DOMEvent -> Eff (dom :: DOM | eff) Number
```


#### `unsafeEventStringProp`

``` purescript
unsafeEventStringProp :: forall eff. String -> DOMEvent -> Eff (dom :: DOM | eff) String
```


#### `unsafeEventBooleanProp`

``` purescript
unsafeEventBooleanProp :: forall eff. String -> DOMEvent -> Eff (dom :: DOM | eff) Boolean
```


#### `unsafeEventView`

``` purescript
unsafeEventView :: forall eff. DOMEvent -> Eff (dom :: DOM | eff) HTMLWindow
```


## Module Data.DOM.Simple.Unsafe.Navigator

#### `unsafeAppName`

``` purescript
unsafeAppName :: forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String
```


#### `unsafeAppVersion`

``` purescript
unsafeAppVersion :: forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String
```


#### `unsafeAppCodeName`

``` purescript
unsafeAppCodeName :: forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String
```


#### `unsafeLanguage`

``` purescript
unsafeLanguage :: forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String
```


#### `unsafePlatform`

``` purescript
unsafePlatform :: forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String
```


#### `unsafeProduct`

``` purescript
unsafeProduct :: forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String
```


#### `unsafeUserAgent`

``` purescript
unsafeUserAgent :: forall eff. DOMNavigator -> Eff (dom :: DOM | eff) String
```



## Module Data.DOM.Simple.Unsafe.NodeList

#### `unsafeNodeListLength`

``` purescript
unsafeNodeListLength :: forall eff. NodeList -> Eff (dom :: DOM | eff) Number
```


#### `unsafeNodeListItem`

``` purescript
unsafeNodeListItem :: forall eff. Number -> NodeList -> Eff (dom :: DOM | eff) HTMLElement
```


#### `unsafeNodeListToArray`

``` purescript
unsafeNodeListToArray :: forall eff. NodeList -> Eff (dom :: DOM | eff) [HTMLElement]
```



## Module Data.DOM.Simple.Unsafe.Sugar

#### `dirtyKindDomRecast`

``` purescript
dirtyKindDomRecast :: forall eff effn a. (Element a) => Eff eff a -> Eff (dom :: DOM | effn) a
```


## Module Data.DOM.Simple.Unsafe.Utils

#### `ensure3`

``` purescript
ensure3 :: forall a. Maybe a -> (a -> Maybe a) -> a -> Maybe a
```


#### `showImpl`

``` purescript
showImpl :: forall a. a -> String
```



## Module Data.DOM.Simple.Unsafe.Window

#### `unsafeDocument`

``` purescript
unsafeDocument :: forall eff a. a -> Eff (dom :: DOM | eff) HTMLDocument
```


#### `unsafeNavigator`

``` purescript
unsafeNavigator :: forall eff a. a -> Eff (dom :: DOM | eff) DOMNavigator
```


#### `unsafeLocation`

``` purescript
unsafeLocation :: forall eff a. a -> Eff (dom :: DOM | eff) DOMLocation
```


#### `unsafeGetLocation`

``` purescript
unsafeGetLocation :: forall eff a. a -> Eff (dom :: DOM | eff) String
```

#### `unsafeSetLocation`

``` purescript
unsafeSetLocation :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeGetSearchLocation`

``` purescript
unsafeGetSearchLocation :: forall eff a. a -> Eff (dom :: DOM | eff) String
```


#### `unsafeSetTimeout`

``` purescript
unsafeSetTimeout :: forall eff b. b -> Number -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Timeout
```


#### `unsafeSetInterval`

``` purescript
unsafeSetInterval :: forall eff b. b -> Number -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Timeout
```


#### `unsafeClearTimeout`

``` purescript
unsafeClearTimeout :: forall eff b. b -> Timeout -> Eff (dom :: DOM | eff) Unit
```


#### `unsafeInnerWidth`

``` purescript
unsafeInnerWidth :: forall eff b. b -> Eff (dom :: DOM | eff) Number
```


#### `unsafeInnerHeight`

``` purescript
unsafeInnerHeight :: forall eff b. b -> Eff (dom :: DOM | eff) Number
```



## Module Data.DOM.Simple.Window

#### `Location`

``` purescript
class Location b where
  getLocation :: forall eff. b -> Eff (dom :: DOM | eff) String
  setLocation :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
  search :: forall eff. b -> Eff (dom :: DOM | eff) String
```


#### `Window`

``` purescript
class Window b where
  document :: forall eff. b -> Eff (dom :: DOM | eff) HTMLDocument
  navigator :: forall eff. b -> Eff (dom :: DOM | eff) DOMNavigator
  location :: forall eff. b -> Eff (dom :: DOM | eff) DOMLocation
  setTimeout :: forall eff. b -> Number -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Timeout
  setInterval :: forall eff. b -> Number -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Timeout
  clearTimeout :: forall eff. b -> Timeout -> Eff (dom :: DOM | eff) Unit
  innerWidth :: forall eff. b -> Eff (dom :: DOM | eff) Number
  innerHeight :: forall eff. b -> Eff (dom :: DOM | eff) Number
```


#### `htmlWindow`

``` purescript
instance htmlWindow :: Window HTMLWindow
```


#### `domLocation`

``` purescript
instance domLocation :: Location DOMLocation
```


#### `globalWindow`

``` purescript
globalWindow :: HTMLWindow
```


#### `getLocationValue`

``` purescript
getLocationValue :: String -> String -> Maybe String
```