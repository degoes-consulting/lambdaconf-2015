module Main where

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Eff
import Control.Monad.Trans
import Data.Tuple
import Debug.Trace

type Stack r t = StateT [Number] (Eff r) t

pop :: forall r. Stack (trace :: Trace | r) Number
pop = do
  (x:xs) <- get
  lift $ trace $ "Popping " ++ show x
  put xs
  return x

push :: forall r. Number -> Stack (trace :: Trace | r) Unit
push x = do
  lift $ trace $ "Pushing " ++ show x
  modify $ (:) x
  return unit

testState :: forall r. Stack (trace :: Trace | r) Number
testState = do
  push 1
  push 2
  push 3
  pop
  pop

main = do
  result <- runStateT testState []
  case result of
    Tuple value state -> do
      print $ "state: " ++ (show state)
      print $ "value: " ++ (show value)
