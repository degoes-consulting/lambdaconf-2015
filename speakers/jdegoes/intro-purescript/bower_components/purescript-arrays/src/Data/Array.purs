-- | Helper functions for working with immutable Javascript arrays.
-- |
-- | _Note_: Depending on your use-case, you may prefer to use `Data.List` or
-- | `Data.Sequence` instead, which might give better performance for certain
-- | use cases. This module is useful when integrating with JavaScript libraries
-- | which use arrays, but immutable arrays are not a practical data structure
-- | for many use cases due to their poor asymptotics.
module Data.Array
  ( (!!)
  , (..)
  , snoc
  , singleton
  , head
  , last
  , tail
  , init
  , null
  , map
  , mapMaybe
  , catMaybes
  , length
  , findIndex
  , findLastIndex
  , elemIndex
  , elemLastIndex
  , append
  , concat
  , reverse
  , drop
  , take
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt
  , deleteBy
  , delete
  , (\\)
  , intersectBy
  , intersect
  , concatMap
  , filter
  , range
  , zipWith
  , nub
  , nubBy
  , sort
  , sortBy
  , group
  , group'
  , groupBy
  , span
  , dropWhile
  , takeWhile
  , replicate
  ) where

import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Data.Maybe
import Prelude.Unsafe (unsafeIndex)

infixl 8 !!

-- | This operator provides a safe way to read a value at a particular index from an array.
-- |
-- | This function returns `Nothing` if the index is out-of-bounds.
-- |
-- | `Data.Array.Unsafe` provides the `unsafeIndex` function, which is an unsafe version of
-- | this function without bounds checking.
(!!) :: forall a. [a] -> Number -> Maybe a
(!!) xs n =
  if n < 0 || n >= (length xs) || isInt n
    then Nothing
    else Just (xs `unsafeIndex` n)
  where
  isInt n = n /= complement (complement n)

-- | Append an element to the end of an array, creating a new array.
foreign import snoc
  "function snoc(l) {\
  \  return function (e) {\
  \    var l1 = l.slice();\
  \    l1.push(e); \
  \    return l1;\
  \  };\
  \}" :: forall a. [a] -> a -> [a]

-- | Create an array of one element
singleton :: forall a. a -> [a]
singleton a = [a]

-- | Get the first element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
head :: forall a. [a] -> Maybe a
head xs = xs !! 0

-- | Get the last element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
last :: forall a. [a] -> Maybe a
last xs = xs !! (length xs - 1)

-- | Get all but the first element of an array, creating a new array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(n)` where `n` is the length of the array
tail :: forall a. [a] -> Maybe [a]
tail (_ : xs) = Just xs
tail _        = Nothing

-- | Get all but the last element of an array, creating a new array, or `Nothing` if the array is empty.
-- |
-- | Running time: `O(n)` where `n` is the length of the array
init :: forall a. [a] -> Maybe [a]
init [] = Nothing
init xs = Just (slice 0 (length xs - 1) xs)

-- | Test whether an array is empty.
null :: forall a. [a] -> Boolean
null [] = true
null _  = false

-- | Get the number of elements in an array
foreign import length
  "function length (xs) {\
  \  return xs.length;\
  \}" :: forall a. [a] -> Number

-- | Find the first index for which a predicate holds,
-- | or `-1` if no such element exists
foreign import findIndex
  "function findIndex (f) {\
  \  return function (arr) {\
  \    for (var i = 0, l = arr.length; i < l; i++) {\
  \      if (f(arr[i])) {\
  \        return i;\
  \      }\
  \    }\
  \    return -1;\
  \  };\
  \}" :: forall a. (a -> Boolean) -> [a] -> Number

-- | Find the last index for which a predicate holds,
-- | or `-1` if no such element exists
foreign import findLastIndex
  "function findLastIndex (f) {\
  \  return function (arr) {\
  \    for (var i = arr.length - 1; i >= 0; i--) {\
  \      if (f(arr[i])) {\
  \        return i;\
  \      }\
  \    }\
  \    return -1;\
  \  };\
  \}" :: forall a. (a -> Boolean) -> [a] -> Number

-- | Find the index of the first element equal to the specified element,
-- | or `-1` if no such element exists
elemIndex :: forall a. (Eq a) => a -> [a] -> Number
elemIndex x = findIndex ((==) x)

-- | Find the index of the last element equal to the specified element,
-- | or `-1` if no such element exists
elemLastIndex :: forall a. (Eq a) => a -> [a] -> Number
elemLastIndex x = findLastIndex ((==) x)

-- | Concatenate two arrays, creating a new array
foreign import append
  "function append (l1) {\
  \  return function (l2) {\
  \    return l1.concat(l2);\
  \  };\
  \}" :: forall a. [a] -> [a] -> [a]

-- | Flatten an array of arrays, creating a new array
foreign import concat
  "function concat (xss) {\
  \  var result = [];\
  \  for (var i = 0, l = xss.length; i < l; i++) {\
  \    result.push.apply(result, xss[i]);\
  \  }\
  \  return result;\
  \}" :: forall a. [[a]] -> [a]

-- | Reverse an array, creating a copy
foreign import reverse
  "function reverse (l) {\
  \  return l.slice().reverse();\
  \}" :: forall a. [a] -> [a]

-- | Drop a number of elements from the start of an array, creating a new array.
foreign import drop
  "function drop (n) {\
  \  return function (l) {\
  \    return l.slice(n);\
  \  };\
  \}" :: forall a. Number -> [a] -> [a]

-- | Keep only a number of elements from the start of an array, creating a new array.
take :: forall a. Number -> [a] -> [a]
take n = slice 0 n

-- | Create a copy of a subarray
foreign import slice
  "function slice (s) {\
  \  return function (e) {\
  \    return function (l) {\
  \      return l.slice(s, e);\
  \    };\
  \  };\
  \}" :: forall a. Number -> Number -> [a] -> [a]

-- | Insert an element at the specified index, creating a new array.
foreign import insertAt
  "function insertAt (index) {\
  \  return function (a) {\
  \    return function (l) {\
  \      var l1 = l.slice();\
  \      l1.splice(index, 0, a);\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> a -> [a] -> [a]

-- | Delete the element at the specified index, creating a new array.
foreign import deleteAt
  "function deleteAt (index) {\
  \  return function (n) {\
  \    return function (l) {\
  \      var l1 = l.slice();\
  \      l1.splice(index, n);\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> Number -> [a] -> [a]

-- | Change the element at the specified index, creating a new array.
foreign import updateAt
  "function updateAt (index) {\
  \  return function (a) {\
  \    return function (l) {\
  \      var i = ~~index;\
  \      if (i < 0 || i >= l.length) return l;\
  \      var l1 = l.slice();\
  \      l1[i] = a;\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> a -> [a] -> [a]

-- | Apply a function to the element at the specified index, creating a new array.
modifyAt :: forall a. Number -> (a -> a) -> [a] -> [a]
modifyAt i f xs = case xs !! i of
                    Just x -> updateAt i (f x) xs
                    Nothing -> xs

-- | Delete the first element of an array which matches the specified value, under the
-- | equivalence relation provided in the first argument, creating a new array.
deleteBy :: forall a. (a -> a -> Boolean) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy eq x ys = case findIndex (eq x) ys of
  i | i < 0 -> ys
  i -> deleteAt i 1 ys

-- | Delete the first element of an array which is equal to the specified value,
-- | creating a new array.
delete :: forall a. (Eq a) => a -> [a] -> [a]
delete = deleteBy (==)

infix 5 \\

-- | Delete the first occurrence of each element in the second array from the first array,
-- | creating a new array.
(\\) :: forall a. (Eq a) => [a] -> [a] -> [a]
(\\) xs ys = go xs ys
  where
  go xs [] = xs
  go [] _  = []
  go xs (y:ys) = go (delete y xs) ys

-- | Calculate the intersection of two arrays, using the specified equivalence relation
-- | to compare elements, creating a new array.
intersectBy :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> [a]
intersectBy _  [] _  = []
intersectBy _  _  [] = []
intersectBy eq xs ys = filter el xs
  where
  el x = findIndex (eq x) ys >= 0

-- | Calculate the intersection of two arrays, creating a new array.
intersect :: forall a. (Eq a) => [a] -> [a] -> [a]
intersect = intersectBy (==)

-- | Apply a function to each element in an array, and flatten the results
-- | into a single, new array.
foreign import concatMap
  "function concatMap (f) {\
  \  return function (arr) {\
  \    var result = [];\
  \    for (var i = 0, l = arr.length; i < l; i++) {\
  \      Array.prototype.push.apply(result, f(arr[i]));\
  \    }\
  \    return result;\
  \  };\
  \}" :: forall a b. (a -> [b]) -> [a] -> [b]

-- | Apply a function to each element in an array, creating a new array.
foreign import map
  "function map (f) {\
  \  return function (arr) {\
  \    var l = arr.length;\
  \    var result = new Array(l);\
  \    for (var i = 0; i < l; i++) {\
  \      result[i] = f(arr[i]);\
  \    }\
  \    return result;\
  \  };\
  \}" :: forall a b. (a -> b) -> [a] -> [b]

-- | Apply a function to each element in an array, keeping only the results which
-- | contain a value, creating a new array.
mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]
mapMaybe f = concatMap (maybe [] singleton <<< f)

-- | Filter an array of optional values, keeping only the elements which contain
-- | a value, creating a new array.
catMaybes :: forall a. [Maybe a] -> [a]
catMaybes = concatMap (maybe [] singleton)

-- | Filter an array, keeping the elements which satisfy a predicate function,
-- | creating a new array.
foreign import filter
  "function filter (f) {\
  \  return function (arr) {\
  \    var n = 0;\
  \    var result = [];\
  \    for (var i = 0, l = arr.length; i < l; i++) {\
  \      if (f(arr[i])) {\
  \        result[n++] = arr[i];\
  \      }\
  \    }\
  \    return result;\
  \  };\
  \}" :: forall a. (a -> Boolean) -> [a] -> [a]

-- | Create an array containing a range of numbers, including both endpoints.
foreign import range
  "function range (start) {\
  \  return function (end) {\
  \    var i = ~~start, e = ~~end;\
  \    var step = i > e ? -1 : 1;\
  \    var result = [i], n = 1;\
  \    while (i !== e) {\
  \      i += step;\
  \      result[n++] = i;\
  \    }\
  \    return result;\
  \  };\
  \}" :: Number -> Number -> [Number]

infix 8 ..

-- | An infix synonym for `range`.
(..) :: Number -> Number -> [Number]
(..) = range

-- | Apply a function to pairs of elements at the same index in two arrays,
-- | collecting the results in a new array.
-- |
-- | If one array is longer, elements will be discarded from the longer array.
-- |
-- | For example
-- |
-- | ```purescript
-- | zipWith (*) [1, 2, 3] [4, 5, 6, 7] == [4, 10, 18]
-- | ```
foreign import zipWith
  "function zipWith (f) {\
  \  return function (xs) {\
  \    return function (ys) {\
  \      var l = xs.length < ys.length ? xs.length : ys.length;\
  \      var result = new Array(l);\
  \      for (var i = 0; i < l; i++) {\
  \        result[i] = f(xs[i])(ys[i]);\
  \      }\
  \      return result;\
  \    };\
  \  };\
  \}" :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]

-- | Remove the duplicates from an array, creating a new array.
nub :: forall a. (Eq a) => [a] -> [a]
nub = nubBy (==)

-- | Remove the duplicates from an array, where element equality is determined by the
-- | specified equivalence relation, creating a new array.
nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]
nubBy _ [] = []
nubBy (==) (x:xs) = x : nubBy (==) (filter (\y -> not (x == y)) xs)

-- | Sort the elements of an array in increasing order, creating a new array.
sort :: forall a. (Ord a) => [a] -> [a]
sort xs = sortBy compare xs

-- | Sort the elements of an array in increasing order, where elements are compared using
-- | the specified partial ordering, creating a new array.
sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy comp xs = sortJS comp' xs
  where
    comp' x y = case comp x y of
      GT -> 1
      EQ -> 0
      LT -> -1

foreign import sortJS
  "function sortJS (f) {\
  \  return function (l) {\
  \    return l.slice().sort(function (x, y) {\
  \      return f(x)(y);\
  \    });\
  \  };\
  \}" :: forall a. (a -> a -> Number) -> [a] -> [a]

-- | Group equal, consecutive elements of an array into arrays.
-- |
-- | For example,
-- |
-- | ```purescript
-- | group [1,1,2,2,1] == [[1,1],[2,2],[1]]
-- | ```
group :: forall a. (Eq a) => [a] -> [[a]]
group xs = groupBy (==) xs

-- | Sort and group the elements of an array into arrays.
-- |
-- | For example,
-- |
-- | ```purescript
-- | group [1,1,2,2,1] == [[1,1,1],[2,2]]
-- | ```
group' :: forall a. (Ord a) => [a] -> [[a]]
group' = group <<< sort

-- | Group equal, consecutive elements of an array into arrays, using the specified
-- | equivalence relation to detemine equality.
groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]
groupBy = go []
  where
  go :: forall a. [[a]] -> (a -> a -> Boolean) -> [a] -> [[a]]
  go acc _  []     = reverse acc
  go acc op (x:xs) = let sp = span (op x) xs in
                     go ((x:sp.init):acc) op sp.rest

-- | Split an array into two parts:
-- |
-- | 1. the longest initial subarray for which all element satisfy the specified predicate
-- | 2. the remaining elements
-- |
-- | For example,
-- |
-- | ```purescript
-- | span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
-- | ```
span :: forall a. (a -> Boolean) -> [a] -> { init :: [a], rest :: [a] }
span = go []
  where
  go :: forall a. [a] -> (a -> Boolean) -> [a] -> { init :: [a], rest :: [a] }
  go acc p (x:xs) | p x = go (x:acc) p xs
  go acc _ xs           = { init: reverse acc, rest: xs }

-- | Calculate the longest initial subarray for which all element satisfy the specified predicate,
-- | creating a new array.
takeWhile :: forall a. (a -> Boolean) -> [a] -> [a]
takeWhile p xs = (span p xs).init

-- | Remove the longest initial subarray for which all element satisfy the specified predicate,
-- | creating a new array.
dropWhile :: forall a. (a -> Boolean) -> [a] -> [a]
dropWhile p xs = (span p xs).rest


-- | Create an array with repeated instances of a value.
foreign import replicate
"""
function replicate(nn) {
  return function(v) {
    var n = nn > 0? nn : 0;
    var r = new Array(n);
    for (var i = 0; i < n; i++)
      r[i] = v;
    return r;
   };
}
""" :: forall a. Number -> a -> [a]

instance functorArray :: Functor [] where
  (<$>) = map

instance applyArray :: Apply [] where
  (<*>) = ap

instance applicativeArray :: Applicative [] where
  pure = singleton

instance bindArray :: Bind [] where
  (>>=) = flip concatMap

instance monadArray :: Monad []

instance semigroupArray :: Semigroup [a] where
  (<>) = append

instance altArray :: Alt [] where
  (<|>) = append

instance plusArray :: Plus [] where
  empty = []

instance alternativeArray :: Alternative []

instance monadPlusArray :: MonadPlus []
