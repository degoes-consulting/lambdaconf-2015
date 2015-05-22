module Main where

import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Debug.Trace

main0 = runContT $ do
  lift (trace "a")
  lift (trace "b")

main = main0 (\_ -> trace "c")
