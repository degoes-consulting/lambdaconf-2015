# Module Documentation

## Module Data.Set


This module defines a type of sets as balanced 2-3 trees, based on
<http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>

Qualified import is encouraged, so as to avoid name clashes with other modules.

#### `Set`

``` purescript
data Set a
```

`Set a` represents a set of values of type `a`

#### `eqSet`

``` purescript
instance eqSet :: (Eq a) => Eq (Set a)
```


#### `showSet`

``` purescript
instance showSet :: (Show a) => Show (Set a)
```


#### `ordSet`

``` purescript
instance ordSet :: (Ord a) => Ord (Set a)
```


#### `empty`

``` purescript
empty :: forall a. Set a
```

An empty set

#### `isEmpty`

``` purescript
isEmpty :: forall a. Set a -> Boolean
```

Test if a set is empty

#### `singleton`

``` purescript
singleton :: forall a. a -> Set a
```

Create a set with one element

#### `checkValid`

``` purescript
checkValid :: forall a. Set a -> Boolean
```

Check whether the underlying tree satisfies the 2-3 invariant

This function is provided for internal use.

#### `member`

``` purescript
member :: forall a. (Ord a) => a -> Set a -> Boolean
```

Test if a value is a member of a set

#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> Set a -> Set a
```

Insert a value into a set

#### `delete`

``` purescript
delete :: forall a. (Ord a) => a -> Set a -> Set a
```

Delete a value from a set

#### `toList`

``` purescript
toList :: forall a. Set a -> [a]
```

Convert a set to an array

#### `fromList`

``` purescript
fromList :: forall a. (Ord a) => [a] -> Set a
```

Create a set from an array of elements

#### `union`

``` purescript
union :: forall a. (Ord a) => Set a -> Set a -> Set a
```

Form the union of two sets

Running time: `O(n * log(m))`

#### `unions`

``` purescript
unions :: forall a. (Ord a) => [Set a] -> Set a
```

Form the union of a collection of sets

#### `difference`

``` purescript
difference :: forall a. (Ord a) => Set a -> Set a -> Set a
```

Form the set difference