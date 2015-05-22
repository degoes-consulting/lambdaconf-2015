# Module Documentation

## Module Control.Monad.Eff.Ref


This module defines an effect and actions for working with
global mutable variables.

_Note_: The `Control.Monad.ST` provides a _safe_ alternative
to global mutable variables when mutation is restricted to a
local scope.

#### `Ref`

``` purescript
data Ref :: !
```

The effect associated with the use of global mutable variables.

#### `RefVal`

``` purescript
data RefVal :: * -> *
```

A value of type `RefVal a` represents a mutable reference
which holds a value of type `a`.

#### `newRef`

``` purescript
newRef :: forall s r. s -> Eff (ref :: Ref | r) (RefVal s)
```

Create a new mutable reference containing the specified value.

#### `readRef`

``` purescript
readRef :: forall s r. RefVal s -> Eff (ref :: Ref | r) s
```

Read the current value of a mutable reference

#### `modifyRef'`

``` purescript
modifyRef' :: forall s b r. RefVal s -> (s -> { retVal :: b, newState :: s }) -> Eff (ref :: Ref | r) b
```

Update the value of a mutable reference by applying a function
to the current value.

#### `modifyRef`

``` purescript
modifyRef :: forall s r. RefVal s -> (s -> s) -> Eff (ref :: Ref | r) Unit
```

Update the value of a mutable reference by applying a function
to the current value.

#### `writeRef`

``` purescript
writeRef :: forall s r. RefVal s -> s -> Eff (ref :: Ref | r) Unit
```

Update the value of a mutable reference to the specified value.


## Module Control.Monad.Eff.Ref.Unsafe


Unsafe functions for working with mutable references.

#### `unsafeRunRef`

``` purescript
unsafeRunRef :: forall eff a. Eff (ref :: Ref | eff) a -> Eff eff a
```

This handler function unsafely removes the `Ref` effect from an
effectful action.

This function might be used when it is impossible to prove to the
typechecker that a particular mutable reference does not escape
its scope.