module Data.List.Unsafe where

import Data.List (List(..))

head :: forall a. List a -> a
head (Cons x _) = x

tail :: forall a. List a -> List a
tail (Cons _ xs) = xs

last :: forall a. List a -> a
last (Cons x Nil) = x
last (Cons _ xs)  = last xs

init :: forall a. List a -> List a
init (Cons x Nil) = Nil
init (Cons x xs)  = Cons x (init xs)
