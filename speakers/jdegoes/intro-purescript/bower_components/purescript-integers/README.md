# Module Documentation

## Module Data.Int

#### `Int`

``` purescript
newtype Int
```


#### `fromNumber`

``` purescript
fromNumber :: Number -> Int
```

Creates an `Int` from a `Number` value. If the value is not already an
integer it is rounded down.

#### `toNumber`

``` purescript
toNumber :: Int -> Number
```


#### `showInt`

``` purescript
instance showInt :: Show Int
```


#### `eqInt`

``` purescript
instance eqInt :: Eq Int
```


#### `ordInt`

``` purescript
instance ordInt :: Ord Int
```


#### `semiringInt`

``` purescript
instance semiringInt :: Semiring Int
```


#### `moduloSemiringInt`

``` purescript
instance moduloSemiringInt :: ModuloSemiring Int
```


#### `ringInt`

``` purescript
instance ringInt :: Ring Int
```



## Module Data.Int.Bits

#### `(.&.)`

``` purescript
(.&.) :: Int -> Int -> Int
```


#### `(.|.)`

``` purescript
(.|.) :: Int -> Int -> Int
```


#### `(.^.)`

``` purescript
(.^.) :: Int -> Int -> Int
```


#### `shl`

``` purescript
shl :: Int -> Int -> Int
```


#### `shr`

``` purescript
shr :: Int -> Int -> Int
```


#### `zshr`

``` purescript
zshr :: Int -> Int -> Int
```


#### `complement`

``` purescript
complement :: Int -> Int
```