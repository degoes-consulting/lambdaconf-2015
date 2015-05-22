module Data.DOM.Simple.Ajax
  ( ReadyState(..)
  , Url(..)
  , HttpMethod(..)
  , ResponseType(..)
  , ArrayBuffer(..)
  , ArrayBufferView(..)
  , Blob(..)
  , FormData(..)
  , HttpData(..)
  , makeXMLHttpRequest
  , readyState
  , onReadyStateChange
  , open
  , send
  , setResponseType
  , responseType
  , response
  , responseText
  , status
  , statusText
  , setRequestHeader
  , getAllResponseHeaders
  , getResponseHeader
  , overrideMimeType
  ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe (Maybe(..))
import DOM

import Data.DOM.Simple.Types
import Data.DOM.Simple.Unsafe.Ajax

data ReadyState = Unsent | Opened | HeadersReceived | Loading | Done

type Url = String

data HttpMethod = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS | JSONP | HttpMethod String

data ResponseType = Default | ArrayBuffer | Blob | Document | Json | Text | MozBlob | MozChunkedText | MozChunkedArrayBuffer

-- These could be given more complete interfaces somewhere:
foreign import data ArrayBuffer :: *
foreign import data ArrayBufferView :: *
foreign import data Blob :: *
foreign import data FormData :: *

data HttpData a
  = NoData
  | TextData String
  | ArrayBufferData ArrayBuffer
  | ArrayBufferViewData ArrayBufferView
  | BlobData Blob
  | FormData FormData
  | DocumentData HTMLDocument -- not necessarily HTML
  | JsonData a

instance showHttpMethod :: Show HttpMethod where
  show GET = "GET"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"
  show PATCH = "PATCH"
  show HEAD = "HEAD"
  show OPTIONS = "OPTIONS"
  show JSONP = "JSONP"
  show (HttpMethod m) = m

instance showResponseType :: Show ResponseType where
  show Default = ""
  show ArrayBuffer = "arraybuffer"
  show Blob = "blob"
  show Document = "document"
  show Json = "json"
  show Text = "text"
  show MozBlob = "moz-blob"
  show MozChunkedText = "moz-chunked-text"
  show MozChunkedArrayBuffer = "moz-chunked-arraybuffer"

foreign import maybeFn
  """
  function maybeFn(nothing, just, a) {
    return a == null ? nothing : just(a);
  }""" :: forall a b. Fn3 b (a -> b) a b

maybe :: forall a. a -> Maybe a
maybe = runFn3 maybeFn Nothing Just

foreign import makeXMLHttpRequest
  """
  function makeXMLHttpRequest() {
    return new XMLHttpRequest();
  }""" :: forall eff. (Eff (dom :: DOM | eff) XMLHttpRequest)

readyState :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) ReadyState
readyState x = do
  r <- unsafeReadyState x
  return $ case r of
    0 -> Unsent
    1 -> Opened
    2 -> HeadersReceived
    3 -> Loading
    4 -> Done

onReadyStateChange :: forall eff e. Eff e Unit -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
onReadyStateChange f x = runFn2 unsafeOnReadyStateChange x f

open :: forall eff. HttpMethod -> Url -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
open m u x = runFn3 unsafeOpen x (show m) u

send :: forall eff a. HttpData a -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
send NoData                  x = runFn1 unsafeSend x
send (TextData a)            x = runFn2 unsafeSendWithPayload x a
send (ArrayBufferData a)     x = runFn2 unsafeSendWithPayload x a
send (ArrayBufferViewData a) x = runFn2 unsafeSendWithPayload x a
send (BlobData a)            x = runFn2 unsafeSendWithPayload x a
send (DocumentData a)        x = runFn2 unsafeSendWithPayload x a
send (FormData a)            x = runFn2 unsafeSendWithPayload x a
send (JsonData a)            x = runFn2 unsafeSendWithPayload x a -- XXX should be encoded

setResponseType :: forall eff. ResponseType -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit
setResponseType rt x = runFn2 unsafeSetResponseType x (show rt)

responseType :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) ResponseType
responseType obj = do
  r <- unsafeResponseType obj
  return $ case r of
    "" -> Default
    "arraybuffer" -> ArrayBuffer
    "blob" -> Blob
    "document" -> Document
    "json" -> Json
    "text" -> Text
    "moz-blob" -> MozBlob
    "moz-chunked-test" -> MozChunkedText
    "moz-chunked-arraybuffer" -> MozChunkedArrayBuffer

response :: forall eff a. XMLHttpRequest -> Eff (dom :: DOM | eff) (HttpData a)
response x = do
  t <- responseType x
  case t of
    Default               -> get TextData
    ArrayBuffer           -> get ArrayBufferData
    Blob                  -> get BlobData
    Document              -> get DocumentData
    Json                  -> get JsonData
    Text                  -> get TextData
    MozBlob               -> get BlobData
    MozChunkedText        -> get TextData
    MozChunkedArrayBuffer -> get ArrayBufferData
  where
    get :: forall eff a o. (a -> HttpData o) -> Eff (dom :: DOM | eff) (HttpData o)
    get t = runFn3 maybeFn NoData t <$> unsafeResponse x

foreign import responseText
  """
  function responseText(obj) {
      return function () {
        return obj.responseText;
    };
  }""" :: forall eff. XMLHttpRequest -> (Eff (dom :: DOM | eff) String)

foreign import status
  """
  function status(obj) {
    return function () {
      return obj.status;
    };
  }""" :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) Number

foreign import statusText
  """
  function statusText(obj) {
      return function () {
        return obj.statusText;
    };
  }""" :: forall eff. XMLHttpRequest -> (Eff (dom :: DOM | eff) String)

foreign import setRequestHeader
  """
  function setRequestHeader(key) {
     return function (value) {
       return function (obj) {
         return function () {
           obj.setRequestHeader(key, value);
           return {};
         };
       };
     };
  }""" :: forall eff. String -> String -> XMLHttpRequest -> (Eff (dom :: DOM | eff) Unit)

foreign import getAllResponseHeaders
  """
  function getAllResponseHeaders(obj) {
    return function () {
      return obj.getAllResponseHeaders();
    };
  }""" :: forall eff. XMLHttpRequest -> (Eff (dom :: DOM | eff) String)

getResponseHeader :: forall eff. String -> XMLHttpRequest -> Eff (dom :: DOM | eff) (Maybe String)
getResponseHeader k x = maybe <$> runFn2 unsafeGetResponseHeader x k

foreign import overrideMimeType
  """
  function overrideMimeType(mime) {
    return function (obj) {
      return function () {
        obj.overrideMimeType(mine);
        return {};
      };
    };
  }""" :: forall eff. String -> XMLHttpRequest -> (Eff (dom :: DOM | eff) Unit)
