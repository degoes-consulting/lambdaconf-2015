# Module Documentation

## Module Data.Map


This module defines a type of maps as balanced 2-3 trees, based on
<http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>

#### `Map`

``` purescript
data Map k v
```

`Map k v` represents maps from keys of type `k` to values of type `v`.

#### `eqMap`

``` purescript
instance eqMap :: (Eq k, Eq v) => Eq (Map k v)
```


#### `showMap`

``` purescript
instance showMap :: (Show k, Show v) => Show (Map k v)
```


#### `ordMap`

``` purescript
instance ordMap :: (Ord k, Ord v) => Ord (Map k v)
```


#### `semigroupMap`

``` purescript
instance semigroupMap :: (Ord k) => Semigroup (Map k v)
```


#### `monoidMap`

``` purescript
instance monoidMap :: (Ord k) => Monoid (Map k v)
```


#### `functorMap`

``` purescript
instance functorMap :: Functor (Map k)
```


#### `foldableMap`

``` purescript
instance foldableMap :: Foldable (Map k)
```


#### `traversableMap`

``` purescript
instance traversableMap :: (Ord k) => Traversable (Map k)
```


#### `showTree`

``` purescript
showTree :: forall k v. (Show k, Show v) => Map k v -> String
```

Render a `Map` as a `String`

#### `empty`

``` purescript
empty :: forall k v. Map k v
```

An empty map

#### `isEmpty`

``` purescript
isEmpty :: forall k v. Map k v -> Boolean
```

Test if a map is empty

#### `singleton`

``` purescript
singleton :: forall k v. k -> v -> Map k v
```

Create a map with one key/value pair

#### `checkValid`

``` purescript
checkValid :: forall k v. Map k v -> Boolean
```

Check whether the underlying tree satisfies the 2-3 invariant

This function is provided for internal use.

#### `lookup`

``` purescript
lookup :: forall k v. (Ord k) => k -> Map k v -> Maybe v
```

Lookup a value for the specified key

#### `member`

``` purescript
member :: forall k v. (Ord k) => k -> Map k v -> Boolean
```

Test if a key is a member of a map

#### `insert`

``` purescript
insert :: forall k v. (Ord k) => k -> v -> Map k v -> Map k v
```

Insert a key/value pair into a map

#### `delete`

``` purescript
delete :: forall k v. (Ord k) => k -> Map k v -> Map k v
```

Delete a key and its corresponding value from a map

#### `alter`

``` purescript
alter :: forall k v. (Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
```

Insert the value, delete a value, or update a value for a key in a map

#### `update`

``` purescript
update :: forall k v. (Ord k) => (v -> Maybe v) -> k -> Map k v -> Map k v
```

Update or delete the value for a key in a map

#### `toList`

``` purescript
toList :: forall k v. Map k v -> [Tuple k v]
```

Convert a map to an array of key/value pairs

#### `fromList`

``` purescript
fromList :: forall k v. (Ord k) => [Tuple k v] -> Map k v
```

Create a map from an array of key/value pairs

#### `fromListWith`

``` purescript
fromListWith :: forall k v. (Ord k) => (v -> v -> v) -> [Tuple k v] -> Map k v
```

Create a map from an array of key/value pairs, using the specified function
to combine values for duplicate keys.

#### `keys`

``` purescript
keys :: forall k v. Map k v -> [k]
```

Get an array of the keys contained in a map

#### `values`

``` purescript
values :: forall k v. Map k v -> [v]
```

Get an array of the values contained in a map

#### `unionWith`

``` purescript
unionWith :: forall k v. (Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
```

Compute the union of two maps, using the specified function
to combine values for duplicate keys.

#### `union`

``` purescript
union :: forall k v. (Ord k) => Map k v -> Map k v -> Map k v
```

Compute the union of two maps, preferring values from the first map in the case
of duplicate keys

#### `unions`

``` purescript
unions :: forall k v. (Ord k) => [Map k v] -> Map k v
```

Compute the union of a collection of maps

#### `map`

``` purescript
map :: forall k a b. (a -> b) -> Map k a -> Map k b
```

Apply a function to the values in a map

#### `size`

``` purescript
size :: forall k v. Map k v -> Number
```

Calculate the number of key/value pairs in a map


## Module Data.StrMap


This module defines a type of native Javascript maps which 
require the keys to be strings.

To maximize performance, Javascript objects are not wrapped,
and some native code is used even when it's not necessary.

#### `StrMap`

``` purescript
data StrMap :: * -> *
```

`StrMap a` represents a map from `String`s to values of type `a`.

#### `thawST`

``` purescript
thawST :: forall a h r. StrMap a -> Eff (st :: ST.ST h | r) (SM.STStrMap h a)
```

Convert an immutable map into a mutable map

#### `freezeST`

``` purescript
freezeST :: forall a h r. SM.STStrMap h a -> Eff (st :: ST.ST h | r) (StrMap a)
```

Convert a mutable map into an immutable map

#### `runST`

``` purescript
runST :: forall a r. (forall h. Eff (st :: ST.ST h | r) (SM.STStrMap h a)) -> Eff r (StrMap a)
```

Freeze a mutable map, creating an immutable map. Use this function as you would use 
`Prelude.runST` to freeze a mutable reference.

The rank-2 type prevents the map from escaping the scope of `runST`.

#### `functorStrMap`

``` purescript
instance functorStrMap :: Functor StrMap
```


#### `fold`

``` purescript
fold :: forall a z. (z -> String -> a -> z) -> z -> StrMap a -> z
```

Fold the keys and values of a map

#### `foldMap`

``` purescript
foldMap :: forall a m. (Monoid m) => (String -> a -> m) -> StrMap a -> m
```

Fold the keys and values of a map, accumulating values using
some `Monoid`.

#### `foldM`

``` purescript
foldM :: forall a m z. (Monad m) => (z -> String -> a -> m z) -> z -> StrMap a -> m z
```

Fold the keys and values of a map, accumulating values and effects in
some `Monad`.

#### `foldableStrMap`

``` purescript
instance foldableStrMap :: Foldable StrMap
```


#### `traversableStrMap`

``` purescript
instance traversableStrMap :: Traversable StrMap
```


#### `foldMaybe`

``` purescript
foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> StrMap a -> z
```

Fold the keys and values of a map.

This function allows the folding function to terminate the fold early,
using `Maybe`.

#### `all`

``` purescript
all :: forall a. (String -> a -> Boolean) -> StrMap a -> Boolean
```

Test whether all key/value pairs in a `StrMap` satisfy a predicate.

#### `eqStrMap`

``` purescript
instance eqStrMap :: (Eq a) => Eq (StrMap a)
```


#### `showStrMap`

``` purescript
instance showStrMap :: (Show a) => Show (StrMap a)
```


#### `empty`

``` purescript
empty :: forall a. StrMap a
```

An empty map

#### `isSubmap`

``` purescript
isSubmap :: forall a. (Eq a) => StrMap a -> StrMap a -> Boolean
```

Test whether one map contains all of the keys and values contained in another map

#### `isEmpty`

``` purescript
isEmpty :: forall a. StrMap a -> Boolean
```

Test whether a map is empty

#### `size`

``` purescript
size :: forall a. StrMap a -> Number
```

Calculate the number of key/value pairs in a map

#### `singleton`

``` purescript
singleton :: forall a. String -> a -> StrMap a
```

Create a map with one key/value pair

#### `lookup`

``` purescript
lookup :: forall a. String -> StrMap a -> Maybe a
```

Lookup the value for a key in a map

#### `member`

``` purescript
member :: forall a. String -> StrMap a -> Boolean
```

Test whether a `String` appears as a key in a map

#### `insert`

``` purescript
insert :: forall a. String -> a -> StrMap a -> StrMap a
```

Insert a key and value into a map

#### `delete`

``` purescript
delete :: forall a. String -> StrMap a -> StrMap a
```

Delete a key and value from a map

#### `alter`

``` purescript
alter :: forall a. (Maybe a -> Maybe a) -> String -> StrMap a -> StrMap a
```

Insert, remove or update a value for a key in a map

#### `update`

``` purescript
update :: forall a. (a -> Maybe a) -> String -> StrMap a -> StrMap a
```

Remove or update a value for a key in a map

#### `fromList`

``` purescript
fromList :: forall a. [Tuple String a] -> StrMap a
```

Create a map from an array of key/value pairs

#### `fromListWith`

``` purescript
fromListWith :: forall a. (a -> a -> a) -> [Tuple String a] -> StrMap a
```

Create a map from an array of key/value pairs, using the specified function
to combine values for duplicate keys.

#### `toList`

``` purescript
toList :: forall a. StrMap a -> [Tuple String a]
```

Convert a map into an array of key/value pairs

#### `keys`

``` purescript
keys :: forall a. StrMap a -> [String]
```

Get an array of the keys in a map

#### `values`

``` purescript
values :: forall a. StrMap a -> [a]
```

Get an array of the values in a map

#### `union`

``` purescript
union :: forall a. StrMap a -> StrMap a -> StrMap a
```

Compute the union of two maps, preferring the first map in the case of 
duplicate keys.

#### `unions`

``` purescript
unions :: forall a. [StrMap a] -> StrMap a
```

Compute the union of a collection of maps

#### `map`

``` purescript
map :: forall a b. (a -> b) -> StrMap a -> StrMap b
```

Map a function over the values in a map

#### `semigroupStrMap`

``` purescript
instance semigroupStrMap :: (Semigroup a) => Semigroup (StrMap a)
```


#### `monoidStrMap`

``` purescript
instance monoidStrMap :: (Semigroup a) => Monoid (StrMap a)
```



## Module Data.StrMap.ST


Helper functions for working with mutable maps using the `ST` effect.

This module can be used when performance is important and mutation is a local effect.

#### `STStrMap`

``` purescript
data STStrMap :: * -> * -> *
```

A reference to a mutable map

The first type parameter represents the memory region which the map belongs to. The second type parameter defines the type of elements of the mutable array.

The runtime representation of a value of type `STStrMap h a` is the same as that of `StrMap a`, except that mutation is allowed.

#### `new`

``` purescript
new :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)
```

Create a new, empty mutable map

#### `peek`

``` purescript
peek :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) a
```

Get the value for a key in a mutable map

#### `poke`

``` purescript
poke :: forall a h r. STStrMap h a -> String -> a -> Eff (st :: ST h | r) (STStrMap h a)
```

Update the value for a key in a mutable map

#### `delete`

``` purescript
delete :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (STStrMap h a)
```

Remove a key and the corresponding value from a mutable map


## Module Data.StrMap.ST.Unsafe

#### `unsafeGet`

``` purescript
unsafeGet :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)
```

Unsafely get the value for a key in a map.

This function does not check whether the key exists in the map.


## Module Data.StrMap.Unsafe

#### `unsafeIndex`

``` purescript
unsafeIndex :: forall a. StrMap a -> String -> a
```

Unsafely get the value for a key in a map.

This function does not check whether the key exists in the map.