# Module Documentation

## Module Math


Wraps the math functions and constants from Javascript's built-in `Math` object.
See [Math Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math).

#### `Radians`

``` purescript
type Radians = Number
```

An alias to make types in this module more explicit.

#### `abs`

``` purescript
abs :: Number -> Number
```

Returns the absolute value of the argument.

#### `acos`

``` purescript
acos :: Number -> Radians
```

Returns the inverse cosine of the argument.

#### `asin`

``` purescript
asin :: Number -> Radians
```

Returns the inverse sine of the argument.

#### `atan`

``` purescript
atan :: Number -> Radians
```

Returns the inverse tangent of the argument.

#### `atan2`

``` purescript
atan2 :: Number -> Number -> Radians
```

Four-quadrant tangent inverse. Given the arguments `y` and `x`, returns
the inverse tangent of `y / x`, where the signs of both arguments are used
to determine the sign of the result.
If the first argument is negative, the result will be negative.
The result is the angle between the positive x axis and  a point `(x, y)`.

#### `ceil`

``` purescript
ceil :: Number -> Number
```

Returns the smallest integer not smaller than the argument.

#### `cos`

``` purescript
cos :: Radians -> Number
```

Returns the cosine of the argument.

#### `exp`

``` purescript
exp :: Number -> Number
```

Returns `e` exponentiated to the power of the argument.

#### `floor`

``` purescript
floor :: Number -> Number
```

Returns the largest integer not larger than the argument.

#### `log`

``` purescript
log :: Number -> Number
```

Returns the natural logarithm of a number.

#### `max`

``` purescript
max :: Number -> Number -> Number
```

Returns the largest of two numbers.

#### `min`

``` purescript
min :: Number -> Number -> Number
```

Returns the smallest of two numbers.

#### `pow`

``` purescript
pow :: Number -> Number -> Number
```

Return  the first argument exponentiated to the power of the second argument.

#### `round`

``` purescript
round :: Number -> Number
```

Returns the integer closest to the argument.

#### `sin`

``` purescript
sin :: Radians -> Number
```

Returns the sine of the argument.

#### `sqrt`

``` purescript
sqrt :: Number -> Number
```

Returns the square root of the argument.

#### `tan`

``` purescript
tan :: Radians -> Number
```

Returns the tangent of the argument.

#### `e`

``` purescript
e :: Number
```

The base of natural logarithms, *e*, around 2.71828.

#### `ln2`

``` purescript
ln2 :: Number
```

The natural logarithm of 2, around 0.6931.

#### `ln10`

``` purescript
ln10 :: Number
```

The natural logarithm of 10, around 2.3025.

#### `log2e`

``` purescript
log2e :: Number
```

The base 2 logarithm of `e`, around 1.4426.

#### `log10e`

``` purescript
log10e :: Number
```

Base 10 logarithm of `e`, around 0.43429.

#### `pi`

``` purescript
pi :: Number
```

The ratio of the circumference of a circle to its diameter, around 3.14159.

#### `sqrt1_2`

``` purescript
sqrt1_2 :: Number
```

The Square root of one half, around 0.707107.

#### `sqrt2`

``` purescript
sqrt2 :: Number
```

The square root of two, around 1.41421.



