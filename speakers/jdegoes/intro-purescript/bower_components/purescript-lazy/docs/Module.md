# Module Documentation

## Module Data.Lazy


A monad for lazily-computed values

#### `Lazy`

``` purescript
data Lazy :: * -> *
```

`Lazy a` represents lazily-computed values of type `a`.

A lazy value is computed at most once - the result is saved
after the first computation, and subsequent attempts to read
the value simply return the saved value.

`Lazy` values can be created with `defer`, or by using the provided
type class instances.

`Lazy` values can be evaluated by using the `force` function.

#### `defer`

``` purescript
defer :: forall a. (Unit -> a) -> Lazy a
```

Defer a computation, creating a `Lazy` value.

#### `force`

``` purescript
force :: forall a. Lazy a -> a
```

Force evaluation of a `Lazy` value.

#### `functorLazy`

``` purescript
instance functorLazy :: Functor Lazy
```


#### `applyLazy`

``` purescript
instance applyLazy :: Apply Lazy
```


#### `applicativeLazy`

``` purescript
instance applicativeLazy :: Applicative Lazy
```


#### `bindLazy`

``` purescript
instance bindLazy :: Bind Lazy
```


#### `monadLazy`

``` purescript
instance monadLazy :: Monad Lazy
```


#### `extendLazy`

``` purescript
instance extendLazy :: Extend Lazy
```


#### `comonadLazy`

``` purescript
instance comonadLazy :: Comonad Lazy
```


#### `eqLazy`

``` purescript
instance eqLazy :: (Eq a) => Eq (Lazy a)
```


#### `ordLazy`

``` purescript
instance ordLazy :: (Ord a) => Ord (Lazy a)
```


#### `showLazy`

``` purescript
instance showLazy :: (Show a) => Show (Lazy a)
```


#### `lazy1Lazy`

``` purescript
instance lazy1Lazy :: CL.Lazy1 Lazy
```



## Module Data.Lazy.List


Lazy linked-lists

#### `List`

``` purescript
data List a
  = Nil 
  | Cons a (Lazy (List a))
```

A lazy linked list type.

This type is strict in its head element, but lazy in its tail.

Various operations on lazy lists require evaluation of the entire list,
so care is needed when defining and using infinite lists.

#### `eqList`

``` purescript
instance eqList :: (Eq a) => Eq (List a)
```


#### `showList`

``` purescript
instance showList :: (Show a) => Show (List a)
```


#### `semigroupList`

``` purescript
instance semigroupList :: Semigroup (List a)
```


#### `monoidList`

``` purescript
instance monoidList :: Monoid (List a)
```


#### `functorList`

``` purescript
instance functorList :: Functor List
```


#### `applyList`

``` purescript
instance applyList :: Apply List
```


#### `applicativeList`

``` purescript
instance applicativeList :: Applicative List
```


#### `bindList`

``` purescript
instance bindList :: Bind List
```


#### `monadList`

``` purescript
instance monadList :: Monad List
```


#### `toArray`

``` purescript
toArray :: forall a. List a -> [a]
```

Convert a lazy list into an immutable array. This function will
attempt to evaluate the entire list, so should only be used on
finite inputs.

Running time: `O(n)` where `n` is the number of elements in the list.

#### `fromArray`

``` purescript
fromArray :: forall a. [a] -> List a
```

Create a lazy list from an immutable array.

Running time: `O(n)` where `n` is the number of elements in the array.

#### `repeat`

``` purescript
repeat :: forall a. a -> List a
```

Create an infinite lazy list which repeats the same value indefinitely.

#### `take`

``` purescript
take :: forall a. Number -> List a -> List a
```

Take the specified number of elements from the start of a lazy list, creating a new
lazy list.

#### `drop`

``` purescript
drop :: forall a. Number -> List a -> List a
```

Drop the specified number of elements from the start of a lazy list, creating a new
lazy list.