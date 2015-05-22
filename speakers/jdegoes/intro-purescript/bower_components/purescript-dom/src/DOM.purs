module DOM where

-- | Effect type for DOM maniupulation
foreign import data DOM :: !

-- | General type for DOM documents.
foreign import data Document :: *

-- | General type for DOM nodes.
foreign import data Node :: *

-- | General type for DOM node lists.
foreign import data NodeList :: *
