module Control.Monad.ListT
  ( ListT()
  , ZipListT()
  , catMaybes
  , cons'
  , drop
  , dropWhile
  , filter
  , foldl
  , foldl'
  , fromArray
  , fromEffect
  , head
  , iterate
  , mapMaybe
  , nil
  , prepend
  , prepend'
  , repeat
  , scanl
  , singleton
  , tail
  , take
  , takeWhile
  , toArray
  , uncons
  , unfold
  , wrapEffect
  , wrapLazy
  , zipWith
  , zipWith'
  , zipList
  ) where 

  import Data.Lazy
  import Data.Monoid
  import Data.Maybe
  import Data.Tuple
  import Data.Unfoldable
  import qualified Data.Array as A

  import Control.Alt
  import Control.Plus
  import Control.Alternative
  import Control.MonadPlus
  import Control.Monad
  import Control.Monad.Trans

  data ListT f a = ListT (f (Step a (ListT f a)))

  newtype ZipListT f a = ZipListT (ListT f a)

  data Step a s =
    Yield a (Lazy s) |
    Skip (Lazy s)    |
    Done

  runListT :: forall f a. ListT f a -> f (Step a (ListT f a))
  runListT (ListT fa) = fa

  nil :: forall f a. (Applicative f) => ListT f a
  nil = ListT $ pure Done

  cons' :: forall f a. (Applicative f) => Lazy a -> Lazy (ListT f a) -> ListT f a
  cons' lh t = ListT $ f <$> (pure unit) where
    f _ = Yield (force lh) t

  prepend' :: forall f a. (Applicative f) => a -> Lazy (ListT f a) -> ListT f a
  prepend' h t = ListT $ pure (Yield h t)

  prepend :: forall f a. (Applicative f) => a -> ListT f a -> ListT f a
  prepend h t = prepend' h (defer $ const t)

  stepMap :: forall f a b. (Functor f) => (Step a (ListT f a) -> Step b (ListT f b)) -> ListT f a -> ListT f b
  stepMap f l = ListT $ f <$> (runListT l)

  concat :: forall f a. (Applicative f) => ListT f a -> ListT f a -> ListT f a
  concat x y = stepMap f x where
    f (Yield a s) = Yield a (flip (<>) y <$> s)
    f (Skip s)    = Skip (flip (<>) y <$> s)
    f Done        = Skip (defer $ const y)

  singleton :: forall f a. (Applicative f) => a -> ListT f a
  singleton a = prepend a nil

  fromEffect :: forall f a. (Applicative f) => f a -> ListT f a
  fromEffect fa = ListT $ (flip Yield) (defer $ \_ -> nil) <$> fa

  wrapEffect :: forall f a. (Monad f) => f (ListT f a) -> ListT f a
  wrapEffect v = ListT $ Skip <<< defer <<< const <$> v

  wrapLazy :: forall f a. (Monad f) => Lazy (ListT f a) -> ListT f a
  wrapLazy v = ListT $ pure (Skip v)

  unfold :: forall f a z. (Monad f) => (z -> f (Maybe (Tuple z a))) -> z -> ListT f a
  unfold f z = ListT $ g <$> f z where
      g (Just (Tuple z a))  = Yield a (defer \_ -> (unfold f z))
      g Nothing             = Done

  iterate :: forall f a. (Monad f) => (a -> a) -> a -> ListT f a
  iterate f a = unfold g a where
    g a = pure $ Just (Tuple (f a) a)

  repeat :: forall f a. (Monad f) => a -> ListT f a
  repeat = iterate id
 
  fromArray :: forall f a. (Monad f) => [a] -> ListT f a
  fromArray xs = unfold f 0 where    
    f n = pure $ Tuple (n + 1) <$> (xs A.!! n)

  toArray :: forall f a. (Monad f) => ListT f a -> f [a]
  toArray = ((<$>) A.reverse) <<< foldl (flip (:)) []

  take :: forall f a. (Applicative f) => Number -> ListT f a -> ListT f a
  take 0 fa = nil
  take n fa = stepMap f fa where
    f (Yield a s) = Yield a s' where s' = take (n - 1) <$> s
    f (Skip s)    = Skip s' where s' = take n <$> s
    f Done        = Done

  takeWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a
  takeWhile f = stepMap g where
    -- FIXME: type inferencer bug with if/then/else
    g (Yield a s) = ifThenElse (f a) (Yield a (takeWhile f <$> s)) Done where ifThenElse p a b = if p then a else b 
    g (Skip s)    = Skip $ takeWhile f <$> s
    g Done        = Done

  drop :: forall f a. (Applicative f) => Number -> ListT f a -> ListT f a
  drop 0 fa = fa
  drop n fa = stepMap f fa where
    f (Yield a s) = Skip s' where s' = drop (n - 1) <$> s
    f (Skip s)    = Skip s' where s' = drop n <$> s
    f Done        = Done

  dropWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a
  dropWhile f = stepMap g where
    g (Yield a s) = if f a then Skip (dropWhile f <$> s) else Yield a s
    g (Skip s)    = Skip $ dropWhile f <$> s
    g Done        = Done

  filter :: forall f a. (Functor f) => (a -> Boolean) -> ListT f a -> ListT f a
  filter f = stepMap g where
    g (Yield a s) = if f a then Yield a s' else Skip s' where s' = filter f <$> s
    g (Skip s)    = Skip s' where s' = filter f <$> s
    g Done        = Done

  mapMaybe :: forall f a b. (Functor f) => (a -> Maybe b) -> ListT f a -> ListT f b
  mapMaybe f = stepMap g where 
    g (Yield a s) = (fromMaybe Skip (Yield <$> (f a))) (mapMaybe f <$> s)
    g (Skip s)    = Skip $ mapMaybe f <$> s
    g Done        = Done

  catMaybes :: forall f a. (Functor f) => ListT f (Maybe a) -> ListT f a
  catMaybes = mapMaybe id

  uncons :: forall f a. (Monad f) => ListT f a -> f (Maybe (Tuple a (ListT f a)))
  uncons l = runListT l >>= g where
    g (Yield a s) = pure $ Just $ Tuple a (force s)
    g (Skip s)    = uncons (force s)
    g Done        = pure Nothing

  head :: forall f a. (Monad f) => ListT f a -> f (Maybe a)
  head l = ((<$>) fst) <$> uncons l

  tail :: forall f a. (Monad f) => ListT f a -> f (Maybe (ListT f a))
  tail l = ((<$>) snd) <$> uncons l

  foldl' :: forall f a b. (Monad f) => (b -> a -> f b) -> b -> ListT f a -> f b
  foldl' f = loop where
    loop b l = uncons l >>= g where
      g Nothing             = pure b
      g (Just (Tuple a as)) = (f b a) >>= (flip loop as)

  foldl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> f b
  foldl f = loop where
    loop b l = uncons l >>= g where
      g Nothing             = pure b
      g (Just (Tuple a as)) = loop (f b a) as

  scanl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> ListT f b
  scanl f b l = unfold g (Tuple b l) where
    g (Tuple b l) = h <$> runListT l where
      h (Yield a s) = Just $ Tuple (Tuple b' (force s)) b' where b' = f b a
      h (Skip s)    = Just $ Tuple (Tuple b (force s)) b
      h Done        = Nothing

  zipWith' :: forall f a b c. (Monad f) => (a -> b -> f c) -> ListT f a -> ListT f b -> ListT f c
  zipWith' f = loop where
    loop fa fb = 
      wrapEffect $ do 
        ua <- uncons fa
        ub <- uncons fb
        g ua ub 
      where g _ Nothing                   = pure nil
            g Nothing _                   = pure nil
            g (Just (Tuple ha ta)) (Just (Tuple hb tb)) = (flip prepend') (defer \_ -> zipWith' f ta tb) <$> (f ha hb)

  zipWith :: forall f a b c. (Monad f) => (a -> b -> c) -> ListT f a -> ListT f b -> ListT f c
  zipWith f = zipWith' g where
    g a b = pure $ f a b

  zipList :: forall f a. ListT f a -> ZipListT f a
  zipList = ZipListT

  instance semigroupListT :: (Applicative f) => Semigroup (ListT f a) where
    (<>) = concat

  instance semigroupZipListT :: (Applicative f) => Semigroup (ZipListT f a) where
    (<>) (ZipListT a) (ZipListT b) = ZipListT $ a <> b

  instance monoidListT :: (Applicative f) => Monoid (ListT f a) where
    mempty = nil

  instance monoidZipListT :: (Applicative f) => Monoid (ZipListT f a) where
    mempty = ZipListT mempty

  instance functorListT :: (Functor f) => Functor (ListT f) where 
    (<$>) f = stepMap g where
      g (Yield a s) = Yield (f a) ((<$>) f <$> s)
      g (Skip s)    = Skip ((<$>) f <$> s)
      g Done        = Done

  instance functorZipListT :: (Functor f) => Functor (ZipListT f) where 
    (<$>) f (ZipListT a) = ZipListT $ f <$> a

  instance unfoldableListT :: (Monad f) => Unfoldable (ListT f) where
    -- unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> ListT f a
    unfoldr f b = go (f b)
      where go Nothing = nil
            go (Just (Tuple a b)) = cons' (pure a) (defer \_ -> (go (f b)))

  instance applyListT :: (Monad f) => Apply (ListT f) where
    (<*>) f x = do
      f' <- f
      x' <- x
      return (f' x')

  instance applyZipListT :: (Monad f) => Apply (ZipListT f) where
    (<*>) (ZipListT a) (ZipListT b) = ZipListT $ zipWith g a b where g f x = f x

  instance applicativeListT :: (Monad f) => Applicative (ListT f) where
    pure = singleton

  instance applicativeZipListT :: (Monad f) => Applicative (ZipListT f) where
    pure = ZipListT <<< pure

  instance bindListT :: (Monad f) => Bind (ListT f) where
    (>>=) fa f = stepMap g fa where
      g (Yield a s) = Skip (h <$> s) where h s = f a `concat` (s >>= f) -- FIXME compiler bug with overlapping instances?
      g (Skip s)    = Skip (h <$> s) where h s = s >>= f
      g Done        = Done

  instance monadListT :: (Monad f) => Monad (ListT f)

  instance monadTransListT :: MonadTrans ListT where
    lift = fromEffect

  instance altListT :: (Applicative f) => Alt (ListT f) where
    (<|>) = concat

  instance altZipListT :: (Applicative f) => Alt (ZipListT f) where
    (<|>) (ZipListT a) (ZipListT b) = ZipListT $ a <|> b

  instance plusListT :: (Monad f) => Plus (ListT f) where
    empty = nil

  instance plusZipListT :: (Monad f) => Plus (ZipListT f) where
    empty = ZipListT empty

  instance alternativeListT :: (Monad f) => Alternative (ListT f)

  instance alternativeZipListT :: (Monad f) => Alternative (ZipListT f)

  instance monadPlusListT :: (Monad f) => MonadPlus (ListT f)
