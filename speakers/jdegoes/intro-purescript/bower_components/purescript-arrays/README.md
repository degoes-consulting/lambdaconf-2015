# Module Documentation

## Module Data.Array


Helper functions for working with immutable Javascript arrays.

_Note_: Depending on your use-case, you may prefer to use `Data.List` or
`Data.Sequence` instead, which might give better performance for certain
use cases. This module is useful when integrating with JavaScript libraries
which use arrays, but immutable arrays are not a practical data structure
for many use cases due to their poor asymptotics.

#### `(!!)`

``` purescript
(!!) :: forall a. [a] -> Number -> Maybe a
```

This operator provides a safe way to read a value at a particular index from an array.

This function returns `Nothing` if the index is out-of-bounds.

`Data.Array.Unsafe` provides the `unsafeIndex` function, which is an unsafe version of
this function without bounds checking.

#### `snoc`

``` purescript
snoc :: forall a. [a] -> a -> [a]
```

Append an element to the end of an array, creating a new array.

#### `singleton`

``` purescript
singleton :: forall a. a -> [a]
```

Create an array of one element

#### `head`

``` purescript
head :: forall a. [a] -> Maybe a
```

Get the first element in an array, or `Nothing` if the array is empty

Running time: `O(1)`.

#### `last`

``` purescript
last :: forall a. [a] -> Maybe a
```

Get the last element in an array, or `Nothing` if the array is empty

Running time: `O(1)`.

#### `tail`

``` purescript
tail :: forall a. [a] -> Maybe [a]
```

Get all but the first element of an array, creating a new array, or `Nothing` if the array is empty

Running time: `O(n)` where `n` is the length of the array

#### `init`

``` purescript
init :: forall a. [a] -> Maybe [a]
```

Get all but the last element of an array, creating a new array, or `Nothing` if the array is empty.

Running time: `O(n)` where `n` is the length of the array

#### `null`

``` purescript
null :: forall a. [a] -> Boolean
```

Test whether an array is empty.

#### `length`

``` purescript
length :: forall a. [a] -> Number
```

Get the number of elements in an array

#### `findIndex`

``` purescript
findIndex :: forall a. (a -> Boolean) -> [a] -> Number
```

Find the first index for which a predicate holds,
or `-1` if no such element exists

#### `findLastIndex`

``` purescript
findLastIndex :: forall a. (a -> Boolean) -> [a] -> Number
```

Find the last index for which a predicate holds,
or `-1` if no such element exists

#### `elemIndex`

``` purescript
elemIndex :: forall a. (Eq a) => a -> [a] -> Number
```

Find the index of the first element equal to the specified element,
or `-1` if no such element exists

#### `elemLastIndex`

``` purescript
elemLastIndex :: forall a. (Eq a) => a -> [a] -> Number
```

Find the index of the last element equal to the specified element,
or `-1` if no such element exists

#### `append`

``` purescript
append :: forall a. [a] -> [a] -> [a]
```

Concatenate two arrays, creating a new array

#### `concat`

``` purescript
concat :: forall a. [[a]] -> [a]
```

Flatten an array of arrays, creating a new array

#### `reverse`

``` purescript
reverse :: forall a. [a] -> [a]
```

Reverse an array, creating a copy

#### `drop`

``` purescript
drop :: forall a. Number -> [a] -> [a]
```

Drop a number of elements from the start of an array, creating a new array.

#### `take`

``` purescript
take :: forall a. Number -> [a] -> [a]
```

Keep only a number of elements from the start of an array, creating a new array.

#### `insertAt`

``` purescript
insertAt :: forall a. Number -> a -> [a] -> [a]
```

Insert an element at the specified index, creating a new array.

#### `deleteAt`

``` purescript
deleteAt :: forall a. Number -> Number -> [a] -> [a]
```

Delete the element at the specified index, creating a new array.

#### `updateAt`

``` purescript
updateAt :: forall a. Number -> a -> [a] -> [a]
```

Change the element at the specified index, creating a new array.

#### `modifyAt`

``` purescript
modifyAt :: forall a. Number -> (a -> a) -> [a] -> [a]
```

Apply a function to the element at the specified index, creating a new array.

#### `deleteBy`

``` purescript
deleteBy :: forall a. (a -> a -> Boolean) -> a -> [a] -> [a]
```

Delete the first element of an array which matches the specified value, under the
equivalence relation provided in the first argument, creating a new array.

#### `delete`

``` purescript
delete :: forall a. (Eq a) => a -> [a] -> [a]
```

Delete the first element of an array which is equal to the specified value,
creating a new array.

#### `(\\)`

``` purescript
(\\) :: forall a. (Eq a) => [a] -> [a] -> [a]
```

Delete the first occurrence of each element in the second array from the first array,
creating a new array.

#### `intersectBy`

``` purescript
intersectBy :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> [a]
```

Calculate the intersection of two arrays, using the specified equivalence relation
to compare elements, creating a new array.

#### `intersect`

``` purescript
intersect :: forall a. (Eq a) => [a] -> [a] -> [a]
```

Calculate the intersection of two arrays, creating a new array.

#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> [b]) -> [a] -> [b]
```

Apply a function to each element in an array, and flatten the results
into a single, new array.

#### `map`

``` purescript
map :: forall a b. (a -> b) -> [a] -> [b]
```

Apply a function to each element in an array, creating a new array.

#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]
```

Apply a function to each element in an array, keeping only the results which
contain a value, creating a new array.

#### `catMaybes`

``` purescript
catMaybes :: forall a. [Maybe a] -> [a]
```

Filter an array of optional values, keeping only the elements which contain
a value, creating a new array.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> [a] -> [a]
```

Filter an array, keeping the elements which satisfy a predicate function,
creating a new array.

#### `range`

``` purescript
range :: Number -> Number -> [Number]
```

Create an array containing a range of numbers, including both endpoints.

#### `(..)`

``` purescript
(..) :: Number -> Number -> [Number]
```

An infix synonym for `range`.

#### `zipWith`

``` purescript
zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
```

Apply a function to pairs of elements at the same index in two arrays,
collecting the results in a new array.

If one array is longer, elements will be discarded from the longer array.

For example

```purescript
zipWith (*) [1, 2, 3] [4, 5, 6, 7] == [4, 10, 18]
```

#### `nub`

``` purescript
nub :: forall a. (Eq a) => [a] -> [a]
```

Remove the duplicates from an array, creating a new array.

#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]
```

Remove the duplicates from an array, where element equality is determined by the
specified equivalence relation, creating a new array.

#### `sort`

``` purescript
sort :: forall a. (Ord a) => [a] -> [a]
```

Sort the elements of an array in increasing order, creating a new array.

#### `sortBy`

``` purescript
sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
```

Sort the elements of an array in increasing order, where elements are compared using
the specified partial ordering, creating a new array.

#### `group`

``` purescript
group :: forall a. (Eq a) => [a] -> [[a]]
```

Group equal, consecutive elements of an array into arrays.

For example,

```purescript
group [1,1,2,2,1] == [[1,1],[2,2],[1]]
```

#### `group'`

``` purescript
group' :: forall a. (Ord a) => [a] -> [[a]]
```

Sort and group the elements of an array into arrays.

For example,

```purescript
group [1,1,2,2,1] == [[1,1,1],[2,2]]
```

#### `groupBy`

``` purescript
groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]
```

Group equal, consecutive elements of an array into arrays, using the specified
equivalence relation to detemine equality.

#### `span`

``` purescript
span :: forall a. (a -> Boolean) -> [a] -> { rest :: [a], init :: [a] }
```

Split an array into two parts:

1. the longest initial subarray for which all element satisfy the specified predicate
2. the remaining elements

For example,

```purescript
span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
```

#### `takeWhile`

``` purescript
takeWhile :: forall a. (a -> Boolean) -> [a] -> [a]
```

Calculate the longest initial subarray for which all element satisfy the specified predicate,
creating a new array.

#### `dropWhile`

``` purescript
dropWhile :: forall a. (a -> Boolean) -> [a] -> [a]
```

Remove the longest initial subarray for which all element satisfy the specified predicate,
creating a new array.

#### `replicate`

``` purescript
replicate :: forall a. Number -> a -> [a]
```

Create an array with repeated instances of a value.

#### `functorArray`

``` purescript
instance functorArray :: Functor Prim.Array
```


#### `applyArray`

``` purescript
instance applyArray :: Apply Prim.Array
```


#### `applicativeArray`

``` purescript
instance applicativeArray :: Applicative Prim.Array
```


#### `bindArray`

``` purescript
instance bindArray :: Bind Prim.Array
```


#### `monadArray`

``` purescript
instance monadArray :: Monad Prim.Array
```


#### `semigroupArray`

``` purescript
instance semigroupArray :: Semigroup [a]
```


#### `altArray`

``` purescript
instance altArray :: Alt Prim.Array
```


#### `plusArray`

``` purescript
instance plusArray :: Plus Prim.Array
```


#### `alternativeArray`

``` purescript
instance alternativeArray :: Alternative Prim.Array
```


#### `monadPlusArray`

``` purescript
instance monadPlusArray :: MonadPlus Prim.Array
```



## Module Data.Array.ST


Helper functions for working with mutable arrays using the `ST` effect.

This module can be used when performance is important and mutation is a local effect.

#### `STArray`

``` purescript
data STArray :: * -> * -> *
```

A reference to a mutable array.

The first type parameter represents the memory region which the array belongs to.
The second type parameter defines the type of elements of the mutable array.

The runtime representation of a value of type `STArray h a` is the same as that of `[a]`,
except that mutation is allowed.

#### `Assoc`

``` purescript
type Assoc a = { index :: Number, value :: a }
```

An element and its index

#### `runSTArray`

``` purescript
runSTArray :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]
```

Freeze a mutable array, creating an immutable array. Use this function as you would use
`runST` to freeze a mutable reference.

The rank-2 type prevents the reference from escaping the scope of `runSTArray`.

#### `emptySTArray`

``` purescript
emptySTArray :: forall a h r. Eff (st :: ST h | r) (STArray h a)
```

Create an empty mutable array.

#### `peekSTArray`

``` purescript
peekSTArray :: forall a h r. STArray h a -> Number -> Eff (st :: ST h | r) (Maybe a)
```

Read the value at the specified index in a mutable array.

#### `pokeSTArray`

``` purescript
pokeSTArray :: forall a h r. STArray h a -> Number -> a -> Eff (st :: ST h | r) Boolean
```

Change the value at the specified index in a mutable array.

#### `pushAllSTArray`

``` purescript
pushAllSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) Number
```

Append the values in an immutable array to the end of a mutable array.

#### `pushSTArray`

``` purescript
pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Number
```

Append an element to the end of a mutable array.

#### `spliceSTArray`

``` purescript
spliceSTArray :: forall a h r. STArray h a -> Number -> Number -> [a] -> Eff (st :: ST h | r) [a]
```

Remove and/or insert elements from/into a mutable array at the specified index.

#### `freeze`

``` purescript
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]
```

Create an immutable copy of a mutable array.

#### `thaw`

``` purescript
thaw :: forall a h r. [a] -> Eff (st :: ST h | r) (STArray h a)
```

Create a mutable copy of an immutable array.

#### `toAssocArray`

``` purescript
toAssocArray :: forall a h r. STArray h a -> Eff (st :: ST h | r) [Assoc a]
```

Create an immutable copy of a mutable array, where each element
is labelled with its index in the original array.


## Module Data.Array.Unsafe


Unsafe helper functions for working with immutable arrays.

_Note_: these functions should be used with care, and may result in unspecified
behavior, including runtime exceptions.

#### `head`

``` purescript
head :: forall a. [a] -> a
```

Get the first element of a non-empty array.

Running time: `O(1)`.

#### `tail`

``` purescript
tail :: forall a. [a] -> [a]
```

Get all but the first element of a non-empty array.

Running time: `O(n)`, where `n` is the length of the array.

#### `last`

``` purescript
last :: forall a. [a] -> a
```

Get the last element of a non-empty array.

Running time: `O(1)`.

#### `init`

``` purescript
init :: forall a. [a] -> [a]
```

Get all but the last element of a non-empty array.

Running time: `O(n)`, where `n` is the length of the array.



