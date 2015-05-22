module Data.Maybe.Unsafe where

import Data.Maybe

-- | A partial function that extracts the value from the `Just` data
-- | constructor. Passing `Nothing` to `fromJust` will throw an error at
-- | runtime.
fromJust :: forall a. Maybe a -> a
fromJust (Just x) = x
