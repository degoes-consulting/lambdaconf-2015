module Data.Either.Unsafe where

import Data.Either

-- | A partial function that extracts the value from the `Left` data constructor.
-- | Passing a `Right` to `fromLeft` will throw an error at runtime.
fromLeft :: forall a b. Either a b -> a
fromLeft (Left a) = a

-- | A partial function that extracts the value from the `Right` data constructor.
-- | Passing a `Left` to `fromRight` will throw an error at runtime.
fromRight :: forall a b. Either a b -> b
fromRight (Right a) = a

