# Module Documentation

## Module Control.Comonad.Trans


This module defines the `ComonadTrans` type class of _comonad transformers_.

#### `ComonadTrans`

``` purescript
class ComonadTrans f where
  lower :: forall w a. (Comonad w) => f w a -> w a
```

The `ComonadTrans` type class represents _comonad transformers_.

A comonad transformer is a type constructor of kind `(* -> *) -> * -> *`, which
takes a `Comonad` as its first argument, and returns another `Comonad`.

This allows us to extend a comonad to provide additional context. By iterating this
process, we create comonad transformer _stacks_, which contain all of the contextual information
required for a particular computation.

The laws state that `lower` is a `Comonad` morphism.

Laws:

- `extract (lower a) = extract a`
- `lower (extend w (f <<< lower)) = extend (lower w) f`