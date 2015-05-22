# Module Documentation

## Module Control.Monad.ListT

#### `ListT`

``` purescript
data ListT f a
```


#### `ZipListT`

``` purescript
newtype ZipListT f a
```


#### `nil`

``` purescript
nil :: forall f a. (Applicative f) => ListT f a
```


#### `cons'`

``` purescript
cons' :: forall f a. (Applicative f) => Lazy a -> Lazy (ListT f a) -> ListT f a
```


#### `prepend'`

``` purescript
prepend' :: forall f a. (Applicative f) => a -> Lazy (ListT f a) -> ListT f a
```


#### `prepend`

``` purescript
prepend :: forall f a. (Applicative f) => a -> ListT f a -> ListT f a
```


#### `singleton`

``` purescript
singleton :: forall f a. (Applicative f) => a -> ListT f a
```


#### `fromEffect`

``` purescript
fromEffect :: forall f a. (Applicative f) => f a -> ListT f a
```


#### `wrapEffect`

``` purescript
wrapEffect :: forall f a. (Monad f) => f (ListT f a) -> ListT f a
```


#### `wrapLazy`

``` purescript
wrapLazy :: forall f a. (Monad f) => Lazy (ListT f a) -> ListT f a
```


#### `unfold`

``` purescript
unfold :: forall f a z. (Monad f) => (z -> f (Maybe (Tuple z a))) -> z -> ListT f a
```


#### `iterate`

``` purescript
iterate :: forall f a. (Monad f) => (a -> a) -> a -> ListT f a
```


#### `repeat`

``` purescript
repeat :: forall f a. (Monad f) => a -> ListT f a
```


#### `fromArray`

``` purescript
fromArray :: forall f a. (Monad f) => [a] -> ListT f a
```


#### `toArray`

``` purescript
toArray :: forall f a. (Monad f) => ListT f a -> f [a]
```


#### `take`

``` purescript
take :: forall f a. (Applicative f) => Number -> ListT f a -> ListT f a
```


#### `takeWhile`

``` purescript
takeWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a
```


#### `drop`

``` purescript
drop :: forall f a. (Applicative f) => Number -> ListT f a -> ListT f a
```


#### `dropWhile`

``` purescript
dropWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a
```


#### `filter`

``` purescript
filter :: forall f a. (Functor f) => (a -> Boolean) -> ListT f a -> ListT f a
```


#### `mapMaybe`

``` purescript
mapMaybe :: forall f a b. (Functor f) => (a -> Maybe b) -> ListT f a -> ListT f b
```


#### `catMaybes`

``` purescript
catMaybes :: forall f a. (Functor f) => ListT f (Maybe a) -> ListT f a
```


#### `uncons`

``` purescript
uncons :: forall f a. (Monad f) => ListT f a -> f (Maybe (Tuple a (ListT f a)))
```


#### `head`

``` purescript
head :: forall f a. (Monad f) => ListT f a -> f (Maybe a)
```


#### `tail`

``` purescript
tail :: forall f a. (Monad f) => ListT f a -> f (Maybe (ListT f a))
```


#### `foldl'`

``` purescript
foldl' :: forall f a b. (Monad f) => (b -> a -> f b) -> b -> ListT f a -> f b
```


#### `foldl`

``` purescript
foldl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> f b
```


#### `scanl`

``` purescript
scanl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> ListT f b
```


#### `zipWith'`

``` purescript
zipWith' :: forall f a b c. (Monad f) => (a -> b -> f c) -> ListT f a -> ListT f b -> ListT f c
```


#### `zipWith`

``` purescript
zipWith :: forall f a b c. (Monad f) => (a -> b -> c) -> ListT f a -> ListT f b -> ListT f c
```


#### `zipList`

``` purescript
zipList :: forall f a. ListT f a -> ZipListT f a
```


#### `semigroupListT`

``` purescript
instance semigroupListT :: (Applicative f) => Semigroup (ListT f a)
```


#### `semigroupZipListT`

``` purescript
instance semigroupZipListT :: (Applicative f) => Semigroup (ZipListT f a)
```


#### `monoidListT`

``` purescript
instance monoidListT :: (Applicative f) => Monoid (ListT f a)
```


#### `monoidZipListT`

``` purescript
instance monoidZipListT :: (Applicative f) => Monoid (ZipListT f a)
```


#### `functorListT`

``` purescript
instance functorListT :: (Functor f) => Functor (ListT f)
```


#### `functorZipListT`

``` purescript
instance functorZipListT :: (Functor f) => Functor (ZipListT f)
```


#### `unfoldableListT`

``` purescript
instance unfoldableListT :: (Monad f) => Unfoldable (ListT f)
```


#### `applyListT`

``` purescript
instance applyListT :: (Monad f) => Apply (ListT f)
```


#### `applyZipListT`

``` purescript
instance applyZipListT :: (Monad f) => Apply (ZipListT f)
```


#### `applicativeListT`

``` purescript
instance applicativeListT :: (Monad f) => Applicative (ListT f)
```


#### `applicativeZipListT`

``` purescript
instance applicativeZipListT :: (Monad f) => Applicative (ZipListT f)
```


#### `bindListT`

``` purescript
instance bindListT :: (Monad f) => Bind (ListT f)
```


#### `monadListT`

``` purescript
instance monadListT :: (Monad f) => Monad (ListT f)
```


#### `monadTransListT`

``` purescript
instance monadTransListT :: MonadTrans ListT
```


#### `altListT`

``` purescript
instance altListT :: (Applicative f) => Alt (ListT f)
```


#### `altZipListT`

``` purescript
instance altZipListT :: (Applicative f) => Alt (ZipListT f)
```


#### `plusListT`

``` purescript
instance plusListT :: (Monad f) => Plus (ListT f)
```


#### `plusZipListT`

``` purescript
instance plusZipListT :: (Monad f) => Plus (ZipListT f)
```


#### `alternativeListT`

``` purescript
instance alternativeListT :: (Monad f) => Alternative (ListT f)
```


#### `alternativeZipListT`

``` purescript
instance alternativeZipListT :: (Monad f) => Alternative (ZipListT f)
```


#### `monadPlusListT`

``` purescript
instance monadPlusListT :: (Monad f) => MonadPlus (ListT f)
```



## Module Data.List

#### `List`

``` purescript
data List a
  = Nil 
  | Cons a (List a)
```


#### `showList`

``` purescript
instance showList :: (Show a) => Show (List a)
```


#### `eqList`

``` purescript
instance eqList :: (Eq a) => Eq (List a)
```


#### `ordList`

``` purescript
instance ordList :: (Ord a) => Ord (List a)
```


#### `semigroupList`

``` purescript
instance semigroupList :: Semigroup (List a)
```


#### `monoidList`

``` purescript
instance monoidList :: Monoid (List a)
```


#### `functorList`

``` purescript
instance functorList :: Functor List
```


#### `foldableList`

``` purescript
instance foldableList :: Foldable List
```


#### `unfoldableList`

``` purescript
instance unfoldableList :: Unfoldable List
```


#### `traversableList`

``` purescript
instance traversableList :: Traversable List
```


#### `applyList`

``` purescript
instance applyList :: Apply List
```


#### `applicativeList`

``` purescript
instance applicativeList :: Applicative List
```


#### `bindList`

``` purescript
instance bindList :: Bind List
```


#### `monadList`

``` purescript
instance monadList :: Monad List
```


#### `altList`

``` purescript
instance altList :: Alt List
```


#### `plusList`

``` purescript
instance plusList :: Plus List
```


#### `alternativeList`

``` purescript
instance alternativeList :: Alternative List
```


#### `monadPlusList`

``` purescript
instance monadPlusList :: MonadPlus List
```


#### `fromArray`

``` purescript
fromArray :: forall a. [a] -> List a
```


#### `toArray`

``` purescript
toArray :: forall a. List a -> [a]
```


#### `(!)`

``` purescript
(!) :: forall a. List a -> Number -> Maybe a
```


#### `drop`

``` purescript
drop :: forall a. Number -> List a -> List a
```


#### `take`

``` purescript
take :: forall a. Number -> List a -> List a
```


#### `length`

``` purescript
length :: forall a. List a -> Number
```


#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> List a -> List a
```


#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
```


#### `catMaybes`

``` purescript
catMaybes :: forall a. List (Maybe a) -> List a
```


#### `head`

``` purescript
head :: forall a. List a -> Maybe a
```


#### `tail`

``` purescript
tail :: forall a. List a -> Maybe (List a)
```


#### `uncons`

``` purescript
uncons :: forall a. List a -> Maybe (Tuple a (List a))
```


#### `last`

``` purescript
last :: forall a. List a -> Maybe a
```


#### `init`

``` purescript
init :: forall a. List a -> Maybe (List a)
```


#### `zipWith`

``` purescript
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
```


#### `null`

``` purescript
null :: forall a. List a -> Boolean
```


#### `span`

``` purescript
span :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)
```


#### `group`

``` purescript
group :: forall a. (Eq a) => List a -> List (List a)
```


#### `groupBy`

``` purescript
groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (List a)
```


#### `(\\)`

``` purescript
(\\) :: forall a. (Eq a) => List a -> List a -> List a
```


#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> List a -> List a
```


#### `insertBy`

``` purescript
insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
```


#### `insertAt`

``` purescript
insertAt :: forall a. Number -> a -> List a -> Maybe (List a)
```


#### `delete`

``` purescript
delete :: forall a. (Eq a) => a -> List a -> List a
```


#### `deleteBy`

``` purescript
deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
```


#### `deleteAt`

``` purescript
deleteAt :: forall a. Number -> List a -> Maybe (List a)
```


#### `alterAt`

``` purescript
alterAt :: forall a. Number -> (a -> Maybe a) -> List a -> Maybe (List a)
```


#### `reverse`

``` purescript
reverse :: forall a. List a -> List a
```


#### `nub`

``` purescript
nub :: forall a. (Eq a) => List a -> List a
```


#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
```


#### `intersect`

``` purescript
intersect :: forall a. (Eq a) => List a -> List a -> List a
```


#### `intersectBy`

``` purescript
intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
```


#### `union`

``` purescript
union :: forall a. (Eq a) => List a -> List a -> List a
```


#### `unionBy`

``` purescript
unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
```



## Module Data.List.Lazy

#### `List`

``` purescript
type List = L.ListT Lazy
```


#### `LazyList`

``` purescript
newtype LazyList a
  = LazyList (List a)
```


#### `unLazyList`

``` purescript
unLazyList :: forall a. LazyList a -> List a
```


#### `foldableLazyList`

``` purescript
instance foldableLazyList :: Foldable LazyList
```



## Module Data.List.Unsafe

#### `head`

``` purescript
head :: forall a. List a -> a
```


#### `tail`

``` purescript
tail :: forall a. List a -> List a
```


#### `last`

``` purescript
last :: forall a. List a -> a
```


#### `init`

``` purescript
init :: forall a. List a -> List a
```



## Module Test.Control.Monad.ListT

#### `arbitraryListT`

``` purescript
instance arbitraryListT :: (Monad f, Arbitrary a) => Arbitrary (ListT f a)
```



## Module Test.Data.List

#### `arbitraryList`

``` purescript
instance arbitraryList :: (Arbitrary a) => Arbitrary (List a)
```