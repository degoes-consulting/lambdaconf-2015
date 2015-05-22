-- | Types for the [WHATWG XMLHttpRequest Living Standard](https://xhr.spec.whatwg.org/#interface-formdata).
module DOM.XHR where

-- | An `XMLHttpRequest` object instance.
foreign import data XMLHttpRequest :: *

-- | A `FormData` object instance.
foreign import data FormData :: *

-- | A `ProgressEvent` object instance.
foreign import data ProgressEvent :: *
