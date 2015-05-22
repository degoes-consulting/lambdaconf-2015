-- | Lazy linked-lists

module Data.Lazy.List 
  ( List(..)
  , toArray
  , fromArray
  , repeat
  , take
  , drop
  ) where

import Data.Array (map)
import Data.Foldable
import Data.Lazy
import Data.Monoid
import Data.Traversable

-- | A lazy linked list type.
-- | 
-- | This type is strict in its head element, but lazy in its tail.
-- |
-- | Various operations on lazy lists require evaluation of the entire list,
-- | so care is needed when defining and using infinite lists.
data List a = Nil | Cons a (Lazy (List a))

instance eqList :: (Eq a) => Eq (List a) where
  (==) Nil         Nil         = true
  (==) (Cons x xs) (Cons y ys) = x == y && (force xs) == (force ys)
  (==) _           _           = false

  (/=) l           l'          =  not (l == l')

instance showList :: (Show a) => Show (List a) where
  show l = "List(" ++ showItems (map show (toArray l)) ++ ")"
  
foreign import showItems
  "function showItems (l) {\
  \  return l.join(', ');\
  \}" :: [String] -> String

instance semigroupList :: Semigroup (List a) where
  (<>) xs ys = xs <.> defer \_ -> ys

(<.>) :: forall a. List a -> Lazy (List a) -> List a
(<.>) Nil         ys = force ys
(<.>) (Cons x xs) ys = Cons x ((\xs' -> xs' <.> ys) <$> xs)

instance monoidList :: Monoid (List a) where
  mempty = Nil
  
instance functorList :: Functor List where
  (<$>) f Nil = Nil
  (<$>) f (Cons h t) = Cons (f h) (((<$>) f) <$> t)

instance applyList :: Apply List where
  (<*>) = ap

instance applicativeList :: Applicative List where
  pure x = Cons x (defer \_ -> Nil)

instance bindList :: Bind List where
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = f x <.> ((\xs' -> xs' >>= f) <$> xs)

instance monadList :: Monad List

-- | Convert a lazy list into an immutable array. This function will
-- | attempt to evaluate the entire list, so should only be used on
-- | finite inputs.
-- | 
-- | Running time: `O(n)` where `n` is the number of elements in the list.
toArray :: forall a. List a -> [a]
toArray Nil = []
toArray (Cons x xs) = x : toArray (force xs)

-- | Create a lazy list from an immutable array.
-- | 
-- | Running time: `O(n)` where `n` is the number of elements in the array.
fromArray :: forall a. [a] -> List a
fromArray = foldr (\x xs -> Cons x (defer \_ -> xs)) Nil

-- | Create an infinite lazy list which repeats the same value indefinitely.
repeat :: forall a. a -> List a
repeat x = Cons x (defer \_ -> repeat x)

-- | Take the specified number of elements from the start of a lazy list, creating a new
-- | lazy list.
take :: forall a. Number -> List a -> List a
take n _ | n < 1   = Nil
take _ Nil         = Nil
take n (Cons x xs) = Cons x (defer \_ -> take (n - 1) (force xs))

-- | Drop the specified number of elements from the start of a lazy list, creating a new
-- | lazy list.
drop :: forall a. Number -> List a -> List a
drop n s | n < 1   = s
drop _ Nil         = Nil
drop n (Cons _ xs) = drop (n - 1) (force xs)
