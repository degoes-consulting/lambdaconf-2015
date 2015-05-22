# Module Documentation

## Module Data.Unfoldable


This module provides a type class for _unfoldable functors_, i.e.
functors which support an `unfoldr` operation.

This allows us to unify various operations on arrays, lists,
sequences, etc.

#### `Unfoldable`

``` purescript
class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a
```

This class identifies data structures which can be _unfolded_,
generalizing `unfoldr` on arrays.

The generating function `f` in `unfoldr f` in understood as follows:

- If `f b` is `Nothing`, then `unfoldr f b` should be empty.
- If `f b` is `Just (Tuple a b1)`, then `unfoldr f b` should consist of `a`
  appended to the result of `unfoldr f b1`.

#### `unfoldableArray`

``` purescript
instance unfoldableArray :: Unfoldable Prim.Array
```