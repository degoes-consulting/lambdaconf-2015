module Data.Traversable
  ( Traversable
  , traverse
  , sequence
  , for
  , zipWithA
  , scanl
  , scanr
  , mapAccumL
  , mapAccumR
  ) where

import Data.Array (zipWith)
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Monoid.Additive
import Data.Monoid.Dual
import Data.Monoid.First
import Data.Monoid.Last
import Data.Monoid.Multiplicative
import Data.Tuple

-- | `Traversable` represents data structures which can be _traversed_,
-- | accumulating results and effects in some `Applicative` functor.
-- |
-- | - `traverse` runs an action for every element in a data structure,
-- |   and accumulates the results.
-- | - `sequence` runs the actions _contained_ in a data structure,
-- |   and accumulates the results.
-- |
-- | The `traverse` and `sequence` functions should be compatible in the
-- | following sense:
-- |
-- | - `traverse f xs = sequence (f <$> xs)`
-- | - `sequence = traverse id` 
-- | 
-- | `Traversable` instances should also be compatible with the corresponding
-- | `Foldable` instances, in the following sense:
-- |
-- | - `foldMap f = runConst <<< traverse (Const <<< f)`
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)

instance traversableArray :: Traversable [] where
  traverse _ []     = pure []
  traverse f (x:xs) = (:) <$> (f x) <*> traverse f xs
  sequence []     = pure []
  sequence (x:xs) = (:) <$> x <*> sequence xs

instance traversableEither :: Traversable (Either a) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right x) = Right <$> f x
  sequence (Left x) = pure (Left x)
  sequence (Right x)  = Right <$> x

instance traversableMaybe :: Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x
  sequence Nothing  = pure Nothing
  sequence (Just x) = Just <$> x

instance traversableTuple :: Traversable (Tuple a) where
  traverse f (Tuple x y) = Tuple x <$> f y
  sequence (Tuple x y) = Tuple x <$> y

instance traversableAdditive :: Traversable Additive where
  traverse f (Additive x) = Additive <$> f x
  sequence (Additive x) = Additive <$> x

instance traversableDual :: Traversable Dual where
  traverse f (Dual x) = Dual <$> f x
  sequence (Dual x) = Dual <$> x

instance traversableFirst :: Traversable First where
  traverse f (First x) = First <$> traverse f x
  sequence (First x) = First <$> sequence x

instance traversableLast :: Traversable Last where
  traverse f (Last x) = Last <$> traverse f x
  sequence (Last x) = Last <$> sequence x

instance traversableMultiplicative :: Traversable Multiplicative where
  traverse f (Multiplicative x) = Multiplicative <$> f x
  sequence (Multiplicative x) = Multiplicative <$> x

-- | A version of `traverse` with its arguments flipped.
-- |
-- | 
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
-- |
-- | For example:
-- |
-- | ```purescript
-- | for [1, 2, 3] \n -> do
-- |   print n
-- |   return (n * n)
-- | ```
for :: forall a b m t. (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
for x f = traverse f x

-- | A generalization of `zipWith` which accumulates results in some `Applicative`
-- | functor.
zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithA f xs ys = sequence (zipWith f xs ys)

newtype StateL s a = StateL (s -> Tuple s a)

stateL :: forall s a. StateL s a -> s -> Tuple s a
stateL (StateL k) = k

instance functorStateL :: Functor (StateL s) where
  (<$>) f k = StateL $ \s -> case stateL k s of
    Tuple s1 a -> Tuple s1 (f a)

instance applyStateL :: Apply (StateL s) where
  (<*>) f x = StateL $ \s -> case stateL f s of
    Tuple s1 f' -> case stateL x s1 of
      Tuple s2 x' -> Tuple s2 (f' x')

instance applicativeStateL :: Applicative (StateL s) where
  pure a = StateL $ \s -> Tuple s a

-- | Fold a data structure from the left, keeping all intermediate results
-- | instead of only the final result.
scanl :: forall a b f. (Traversable f) => (b -> a -> b) -> b -> f a -> f b
scanl f b0 xs = snd $ mapAccumL (\b a -> let b' = f b a in Tuple b' b') b0 xs

-- | Fold a data structure from the left, keeping all intermediate results
-- | instead of only the final result.
-- |
-- | Unlike `scanl`, `mapAccumL` allows the type of accumulator to differ
-- | from the element type of the final data structure.
mapAccumL :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)
mapAccumL f s0 xs = stateL (traverse (\a -> StateL $ \s -> f s a) xs) s0

newtype StateR s a = StateR (s -> Tuple s a)

stateR :: forall s a. StateR s a -> s -> Tuple s a
stateR (StateR k) = k

instance functorStateR :: Functor (StateR s) where
  (<$>) f k = StateR $ \s -> case stateR k s of
    Tuple s1 a -> Tuple s1 (f a)

instance applyStateR :: Apply (StateR s) where
  (<*>) f x = StateR $ \s -> case stateR x s of
    Tuple s1 x' -> case stateR f s1 of
      Tuple s2 f' -> Tuple s2 (f' x')

instance applicativeStateR :: Applicative (StateR s) where
  pure a = StateR $ \s -> Tuple s a

-- | Fold a data structure from the right, keeping all intermediate results
-- | instead of only the final result.
scanr :: forall a b f. (Traversable f) => (a -> b -> b) -> b -> f a -> f b
scanr f b0 xs = snd $ mapAccumR (\b a -> let b' = f a b in Tuple b' b') b0 xs

-- | Fold a data structure from the right, keeping all intermediate results
-- | instead of only the final result.
-- |
-- | Unlike `scanr`, `mapAccumR` allows the type of accumulator to differ
-- | from the element type of the final data structure.
mapAccumR :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)
mapAccumR f s0 xs = stateR (traverse (\a -> StateR $ \s -> f s a) xs) s0
