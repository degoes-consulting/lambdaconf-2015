# Module Documentation

## Module Data.Maybe

#### `Maybe`

``` purescript
data Maybe a
  = Nothing 
  | Just a
```

The `Maybe` type is used to represent optional values and can be seen as
something like a type-safe `null`, where `Nothing` is `null` and `Just x`
is the non-null value `x`.

#### `maybe`

``` purescript
maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
```

Takes a default value, a function, and a `Maybe` value. If the `Maybe`
value is `Nothing` the default value is returned, otherwise the function
is applied to the value inside the `Just` and the result is returned.

``` purescript
maybe x f Nothing == x
maybe x f (Just y) == f y
```

#### `fromMaybe`

``` purescript
fromMaybe :: forall a. a -> Maybe a -> a
```

Takes a default value, and a `Maybe` value. If the `Maybe` value is
`Nothing` the default value is returned, otherwise the value inside the
`Just` is returned.

``` purescript
fromMaybe x Nothing == x
fromMaybe x (Just y) == y
```

#### `isJust`

``` purescript
isJust :: forall a. Maybe a -> Boolean
```

Returns `true` when the `Maybe` value was constructed with `Just`.

#### `isNothing`

``` purescript
isNothing :: forall a. Maybe a -> Boolean
```

Returns `true` when the `Maybe` value is `Nothing`.

#### `functorMaybe`

``` purescript
instance functorMaybe :: Functor Maybe
```

The `Functor` instance allows functions to transform the contents of a
`Just` with the `<$>` operator:

``` purescript
f <$> Just x == Just (f x)
```

`Nothing` values are left untouched:

``` purescript
f <$> Nothing == Nothing
```

#### `applyMaybe`

``` purescript
instance applyMaybe :: Apply Maybe
```

The `Apply` instance allows functions contained within a `Just` to
transform a value contained within a `Just` using the `(<*>)` operator:

``` purescript
Just f <*> Just x == Just (f x)
```

`Nothing` values are left untouched:

``` purescript
Just f <*> Nothing == Nothing
Nothing <*> Just x == Nothing
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used transform a
pure function to take `Maybe`-typed arguments so `f :: a -> b -> c`
becomes `f :: Maybe a -> Maybe b -> Maybe c`:

``` purescript
f <$> Just x <*> Just y == Just (f x y)
```

The `Nothing`-preserving behaviour of both operators means the result of
an expression like the above but where any one of the values is `Nothing`
means the whole result becomes `Nothing` also:

``` purescript
f <$> Nothing <*> Just y == Nothing
f <$> Just x <*> Nothing == Nothing
f <$> Nothing <*> Nothing == Nothing
```

#### `applicativeMaybe`

``` purescript
instance applicativeMaybe :: Applicative Maybe
```

The `Applicative` instance enables lifting of values into `Maybe` with the
`pure` or `return` function (`return` is an alias for `pure`):

``` purescript
pure x :: Maybe _ == Just x
return x :: Maybe _ == Just x
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
`pure` can be used to pass a mixture of `Maybe` and non-`Maybe` typed
values to a function that does not usually expect them, by using `pure`
for any value that is not already `Maybe` typed:

``` purescript
f <$> Just x <*> pure y == Just (f x y)
```

Even though `pure = Just` it is recommended to use `pure` in situations
like this as it allows the choice of `Applicative` to be changed later
without having to go through and replace `Just` with a new constructor.

#### `altMaybe`

``` purescript
instance altMaybe :: Alt Maybe
```

The `Alt` instance allows for a choice to be made between two `Maybe`
values with the `<|>` operator, where the first `Just` encountered
is taken.

``` purescript
Just x <|> Just y == Just x
Nothing <|> Just y == Just y
Nothing <|> Nothing == Nothing
```

#### `plusMaybe`

``` purescript
instance plusMaybe :: Plus Maybe
```

The `Plus` instance provides a default `Maybe` value:

``` purescript
empty :: Maybe _ == Nothing
```

#### `alternativeMaybe`

``` purescript
instance alternativeMaybe :: Alternative Maybe
```

The `Alternative` instance guarantees that there are both `Applicative` and
`Plus` instances for `Maybe`.

#### `bindMaybe`

``` purescript
instance bindMaybe :: Bind Maybe
```

The `Bind` instance allows sequencing of `Maybe` values and functions that
return a `Maybe` by using the `>>=` operator:

``` purescript
Just x >>= f = f x
Nothing >>= f = Nothing
```

#### `monadMaybe`

``` purescript
instance monadMaybe :: Monad Maybe
```

The `Monad` instance guarantees that there are both `Applicative` and
`Bind` instances for `Maybe`. This also enables the `do` syntactic sugar:

``` purescript
do
  x' <- x
  y' <- y
  pure (f x' y')
```

Which is equivalent to:

``` purescript
x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
```

#### `monadPlusMaybe`

``` purescript
instance monadPlusMaybe :: MonadPlus Maybe
```

The `MonadPlus` instance guarantees that there are both `Monad` and
`Alternative` instances for `Maybe`.

#### `extendMaybe`

``` purescript
instance extendMaybe :: Extend Maybe
```

The `Extend` instance allows sequencing of `Maybe` values and functions
that accept a `Maybe a` and return a non-`Maybe` result using the
`<<=` operator.

``` purescript
f <<= Nothing = Nothing
f <<= Just x = Just (f x)
```

#### `semigroupMaybe`

``` purescript
instance semigroupMaybe :: (Semigroup a) => Semigroup (Maybe a)
```

The `Semigroup` instance enables use of the operator `<>` on `Maybe` values
whenever there is a `Semigroup` instance for the type the `Maybe` contains.
The exact behaviour of `<>` depends on the "inner" `Semigroup` instance,
but generally captures the notion of appending or combining things.

``` purescript
Just x <> Just y = Just (x <> y)
Just x <> Nothing = Just x
Nothing <> Just y = Just y
Nothing <> Nothing = Nothing
```

#### `showMaybe`

``` purescript
instance showMaybe :: (Show a) => Show (Maybe a)
```

The `Show` instance allows `Maybe` values to be rendered as a string with
`show` whenever there is an `Show` instance for the type the `Maybe`
contains.

#### `eqMaybe`

``` purescript
instance eqMaybe :: (Eq a) => Eq (Maybe a)
```

The `Eq` instance allows `Maybe` values to be checked for equality with
`==` and inequality with `/=` whenever there is an `Eq` instance for the
type the `Maybe` contains.

#### `ordMaybe`

``` purescript
instance ordMaybe :: (Ord a) => Ord (Maybe a)
```

The `Ord` instance allows `Maybe` values to be compared with
`compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
the type the `Maybe` contains.

`Nothing` is considered to be less than any `Just` value.


## Module Data.Maybe.Unsafe

#### `fromJust`

``` purescript
fromJust :: forall a. Maybe a -> a
```

A partial function that extracts the value from the `Just` data
constructor. Passing `Nothing` to `fromJust` will throw an error at
runtime.