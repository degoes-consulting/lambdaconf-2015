# Module Documentation

## Module Data.Foldable

#### `Foldable`

``` purescript
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m
```

`Foldable` represents data structures which can be _folded_.

- `foldr` folds a structure from the right
- `foldl` folds a structure from the left
- `foldMap` folds a structure by accumulating values in a `Monoid`

#### `foldableArray`

``` purescript
instance foldableArray :: Foldable Prim.Array
```


#### `foldableEither`

``` purescript
instance foldableEither :: Foldable (Either a)
```


#### `foldableMaybe`

``` purescript
instance foldableMaybe :: Foldable Maybe
```


#### `foldableTuple`

``` purescript
instance foldableTuple :: Foldable (Tuple a)
```


#### `foldableAdditive`

``` purescript
instance foldableAdditive :: Foldable Additive
```


#### `foldableDual`

``` purescript
instance foldableDual :: Foldable Dual
```


#### `foldableFirst`

``` purescript
instance foldableFirst :: Foldable First
```


#### `foldableLast`

``` purescript
instance foldableLast :: Foldable Last
```


#### `foldableMultiplicative`

``` purescript
instance foldableMultiplicative :: Foldable Multiplicative
```


#### `fold`

``` purescript
fold :: forall f m. (Foldable f, Monoid m) => f m -> m
```

Fold a data structure, accumulating values in some `Monoid`.

#### `traverse_`

``` purescript
traverse_ :: forall a b f m. (Applicative m, Foldable f) => (a -> m b) -> f a -> m Unit
```

Traverse a data structure, performing some effects encoded by an
`Applicative` functor at each value, ignoring the final result.

For example:

```purescript
traverse_ print [1, 2, 3]
```

#### `for_`

``` purescript
for_ :: forall a b f m. (Applicative m, Foldable f) => f a -> (a -> m b) -> m Unit
```

A version of `traverse_` with its arguments flipped.

This can be useful when running an action written using do notation
for every element in a data structure:

For example:

```purescript
for_ [1, 2, 3] \n -> do
  print n
  trace "squared is"
  print (n * n)
```

#### `sequence_`

``` purescript
sequence_ :: forall a f m. (Applicative m, Foldable f) => f (m a) -> m Unit
```

Perform all of the effects in some data structure in the order
given by the `Foldable` instance, ignoring the final result.

For example:

```purescript
sequence_ [ trace "Hello, ", trace " world!" ]
```

#### `mconcat`

``` purescript
mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m
```

Fold a data structure, accumulating values in some `Monoid`.

#### `intercalate`

``` purescript
intercalate :: forall f m. (Foldable f, Monoid m) => m -> f m -> m
```

Fold a data structure, accumulating values in some `Monoid`,
combining adjacent elements using the specified separator. 

#### `and`

``` purescript
and :: forall f. (Foldable f) => f Boolean -> Boolean
```

Test whether all `Boolean` values in a data structure are `true`.

#### `or`

``` purescript
or :: forall f. (Foldable f) => f Boolean -> Boolean
```

Test whether any `Boolean` value in a data structure is `true`.

#### `any`

``` purescript
any :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean
```

Test whether a predicate holds for any element in a data structure.

#### `all`

``` purescript
all :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean
```

Test whether a predicate holds for all elements in a data structure.

#### `sum`

``` purescript
sum :: forall f. (Foldable f) => f Number -> Number
```

Find the sum of the numeric values in a data structure.

#### `product`

``` purescript
product :: forall f. (Foldable f) => f Number -> Number
```

Find the product of the numeric values in a data structure.

#### `elem`

``` purescript
elem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean
```

Test whether a value is an element of a data structure.

#### `notElem`

``` purescript
notElem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean
```

Test whether a value is not an element of a data structure.

#### `find`

``` purescript
find :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Maybe a
```

Try to find an element in a data structure which satisfies a predicate.

#### `lookup`

``` purescript
lookup :: forall a b f. (Eq a, Foldable f) => a -> f (Tuple a b) -> Maybe b
```

Lookup a value in a data structure of `Tuple`s, generalizing association lists.

#### `foldrArray`

``` purescript
foldrArray :: forall a b. (a -> b -> b) -> b -> [a] -> b
```


#### `foldlArray`

``` purescript
foldlArray :: forall a b. (b -> a -> b) -> b -> [a] -> b
```



## Module Data.Traversable

#### `Traversable`

``` purescript
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)
```

`Traversable` represents data structures which can be _traversed_,
accumulating results and effects in some `Applicative` functor.

- `traverse` runs an action for every element in a data structure,
  and accumulates the results.
- `sequence` runs the actions _contained_ in a data structure,
  and accumulates the results.

The `traverse` and `sequence` functions should be compatible in the
following sense:

- `traverse f xs = sequence (f <$> xs)`
- `sequence = traverse id` 

`Traversable` instances should also be compatible with the corresponding
`Foldable` instances, in the following sense:

- `foldMap f = runConst <<< traverse (Const <<< f)`

#### `traversableArray`

``` purescript
instance traversableArray :: Traversable Prim.Array
```


#### `traversableEither`

``` purescript
instance traversableEither :: Traversable (Either a)
```


#### `traversableMaybe`

``` purescript
instance traversableMaybe :: Traversable Maybe
```


#### `traversableTuple`

``` purescript
instance traversableTuple :: Traversable (Tuple a)
```


#### `traversableAdditive`

``` purescript
instance traversableAdditive :: Traversable Additive
```


#### `traversableDual`

``` purescript
instance traversableDual :: Traversable Dual
```


#### `traversableFirst`

``` purescript
instance traversableFirst :: Traversable First
```


#### `traversableLast`

``` purescript
instance traversableLast :: Traversable Last
```


#### `traversableMultiplicative`

``` purescript
instance traversableMultiplicative :: Traversable Multiplicative
```


#### `for`

``` purescript
for :: forall a b m t. (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
```

A version of `traverse` with its arguments flipped.


This can be useful when running an action written using do notation
for every element in a data structure:

For example:

```purescript
for [1, 2, 3] \n -> do
  print n
  return (n * n)
```

#### `zipWithA`

``` purescript
zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
```

A generalization of `zipWith` which accumulates results in some `Applicative`
functor.

#### `functorStateL`

``` purescript
instance functorStateL :: Functor (StateL s)
```


#### `applyStateL`

``` purescript
instance applyStateL :: Apply (StateL s)
```


#### `applicativeStateL`

``` purescript
instance applicativeStateL :: Applicative (StateL s)
```


#### `scanl`

``` purescript
scanl :: forall a b f. (Traversable f) => (b -> a -> b) -> b -> f a -> f b
```

Fold a data structure from the left, keeping all intermediate results
instead of only the final result.

#### `mapAccumL`

``` purescript
mapAccumL :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)
```

Fold a data structure from the left, keeping all intermediate results
instead of only the final result.

Unlike `scanl`, `mapAccumL` allows the type of accumulator to differ
from the element type of the final data structure.

#### `functorStateR`

``` purescript
instance functorStateR :: Functor (StateR s)
```


#### `applyStateR`

``` purescript
instance applyStateR :: Apply (StateR s)
```


#### `applicativeStateR`

``` purescript
instance applicativeStateR :: Applicative (StateR s)
```


#### `scanr`

``` purescript
scanr :: forall a b f. (Traversable f) => (a -> b -> b) -> b -> f a -> f b
```

Fold a data structure from the right, keeping all intermediate results
instead of only the final result.

#### `mapAccumR`

``` purescript
mapAccumR :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)
```

Fold a data structure from the right, keeping all intermediate results
instead of only the final result.

Unlike `scanr`, `mapAccumR` allows the type of accumulator to differ
from the element type of the final data structure.