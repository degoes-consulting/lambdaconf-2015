module Main where

import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans
import Data.Tuple
import Debug.Trace

testWriter :: Writer String Number
testWriter = do
  tell "Hello from testWriter"
  return 42

main = case runWriter testWriter of
  Tuple value output -> do
    print $ output
    print $ value
