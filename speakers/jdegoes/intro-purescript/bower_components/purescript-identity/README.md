# Module Documentation

## Module Data.Identity

#### `Identity`

``` purescript
newtype Identity a
  = Identity a
```


#### `runIdentity`

``` purescript
runIdentity :: forall a. Identity a -> a
```


#### `eqIdentity`

``` purescript
instance eqIdentity :: (Eq a) => Eq (Identity a)
```


#### `ordIdentity`

``` purescript
instance ordIdentity :: (Ord a) => Ord (Identity a)
```


#### `showConst`

``` purescript
instance showConst :: (Show a) => Show (Identity a)
```


#### `functorIdentity`

``` purescript
instance functorIdentity :: Functor Identity
```


#### `applyIdentity`

``` purescript
instance applyIdentity :: Apply Identity
```


#### `applicativeIdentity`

``` purescript
instance applicativeIdentity :: Applicative Identity
```


#### `bindIdentity`

``` purescript
instance bindIdentity :: Bind Identity
```


#### `monadIdentity`

``` purescript
instance monadIdentity :: Monad Identity
```


#### `extendIdentity`

``` purescript
instance extendIdentity :: Extend Identity
```


#### `comonadIdentity`

``` purescript
instance comonadIdentity :: Comonad Identity
```


#### `foldableIdentity`

``` purescript
instance foldableIdentity :: Foldable Identity
```


#### `traversableIdentity`

``` purescript
instance traversableIdentity :: Traversable Identity
```