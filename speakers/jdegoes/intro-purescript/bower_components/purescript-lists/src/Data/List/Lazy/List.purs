module Data.List.Lazy
  ( List(..)
  , LazyList(..)
  , catMaybes
  , cons'
  , drop
  , dropWhile
  , filter
  , fromArray
  , fromEffect
  , head
  , iterate
  , mapMaybe
  , nil
  , prepend
  , prepend'
  , repeat
  , singleton
  , tail
  , take
  , takeWhile
  , toArray
  , uncons
  , unfold
  , unLazyList
  , wrapEffect
  , wrapLazy
  , zipWith
  , zipWith'
  ) where

  import Data.Lazy
  import Data.Maybe
  import Data.Tuple
  import Data.Monoid
  import Data.Foldable
  import Data.Unfoldable
  import Data.Traversable

  import qualified Control.Monad.ListT as L

  type List = L.ListT Lazy

  newtype LazyList a = LazyList (List a)

  unLazyList :: forall a. LazyList a -> List a
  unLazyList (LazyList l) = l

  catMaybes   = L.catMaybes
  cons'       = L.cons'
  drop        = L.drop
  dropWhile   = L.dropWhile
  filter      = L.filter
  fromArray   = L.fromArray
  fromEffect  = L.fromEffect
  head        = L.head
  iterate     = L.iterate
  mapMaybe    = L.mapMaybe
  nil         = L.nil
  prepend     = L.prepend
  prepend'    = L.prepend'
  repeat      = L.repeat
  singleton   = L.singleton
  tail        = L.tail
  take        = L.take
  takeWhile   = L.takeWhile
  toArray     = L.toArray
  uncons      = L.uncons
  unfold      = L.unfold
  wrapEffect  = L.wrapEffect
  wrapLazy    = L.wrapLazy
  zipWith     = L.zipWith
  zipWith'    = L.zipWith'

  instance foldableLazyList :: Foldable LazyList where
    -- foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
    foldr f b (LazyList l) = force $ foldr' b l
      where foldr' b l =  let g Nothing            = pure b
                              g (Just (Tuple a l)) = f a <$> foldr' b l

                          in  uncons l >>= g 

    -- foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
    foldl f b (LazyList l) = foldl' b l
      where foldl' b l =  let g Nothing             = b
                              g (Just (Tuple a l))  = foldl' (f b a) l

                          in  g (force (uncons l))

    -- foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m 
    foldMap f (LazyList l) = foldMap' l 
      where foldMap' l =  let g Nothing             = mempty
                              g (Just (Tuple a l))  = f a <> foldMap' l

                          in  g (force (uncons l))
