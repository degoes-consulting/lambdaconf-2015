module Main where

import Data.Tuple
import Data.Maybe
import Data.Array
import Data.Unfoldable

import Debug.Trace

collatz :: Number -> [Number]
collatz = unfoldr step 
  where
  step 1 = Nothing
  step n = Just $ Tuple n $ if n % 2 == 0 then n / 2 else n * 3 + 1

main = print $ collatz 1000
