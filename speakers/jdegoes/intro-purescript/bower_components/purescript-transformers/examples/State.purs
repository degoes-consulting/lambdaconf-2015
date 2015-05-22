module Main where

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Data.Tuple
import Debug.Trace

incState :: forall eff a. State Number Unit
incState = modify $ (+) 1

testState :: forall eff a. State Number String
testState = do
  incState
  incState
  incState
  incState
  incState
  incState
  return "Done"

main = case runState testState 0 of
  Tuple value state -> do
    print $ "state: " ++ (show state)
    print $ "value: " ++ (show value)
