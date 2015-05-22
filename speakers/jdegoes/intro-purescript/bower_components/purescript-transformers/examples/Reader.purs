module Main where

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Debug.Trace

testReader :: Reader String String
testReader = local (\x -> x ++ "!") ask

main = do
  print $ runReader testReader "Done"
