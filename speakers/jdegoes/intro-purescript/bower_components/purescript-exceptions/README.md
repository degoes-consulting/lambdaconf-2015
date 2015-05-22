# Module Documentation

## Module Control.Monad.Eff.Exception


This module defines an effect, actions and handlers for working
with Javascript exceptions.

#### `Exception`

``` purescript
data Exception :: !
```

This effect is used to annotate code which possibly throws exceptions

#### `Error`

``` purescript
data Error :: *
```

The type of Javascript errors

#### `showError`

``` purescript
instance showError :: Show Error
```


#### `error`

``` purescript
error :: String -> Error
```

Create a Javascript error, specifying a message

#### `message`

``` purescript
message :: Error -> String
```

Get the error message from a Javascript error

#### `throwException`

``` purescript
throwException :: forall a eff. Error -> Eff (err :: Exception | eff) a
```

Throw an exception

For example:

```purescript
main = do
  x <- readNumber
  when (x < 0) $ throwException $ 
    error "Expected a non-negative number"
```

#### `catchException`

``` purescript
catchException :: forall a eff. (Error -> Eff eff a) -> Eff (err :: Exception | eff) a -> Eff eff a
```

Catch an exception by providing an exception handler.

This handler removes the `Exception` effect.

For example:

```purescript
main = catchException print do
  trace "Exceptions thrown in this block will be logged to the console"
```