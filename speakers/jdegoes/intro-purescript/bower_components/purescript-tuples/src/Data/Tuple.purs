-- | A data type and functions for working with ordered pairs and sequences of values.
module Data.Tuple where
  import Control.Comonad
  import Control.Extend
  import Control.Lazy

  import Data.Array
  import Data.Monoid

  -- | A simple product type for wrapping a pair of component values.
  data Tuple a b = Tuple a b

  -- | Allows `Tuple`s to be rendered as a string with `show` whenever there are
  -- | `Show` instances for both component types.
  instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
    show (Tuple a b) = "Tuple (" ++ show a ++ ") (" ++ show b ++ ")"

  -- | Allows `Tuple`s to be checked for equality with `==` and `/=` whenever
  -- | there are `Eq` instances for both component types.
  instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2
    (/=) t1 t2 = not (t1 == t2)

  -- | Allows `Tuple`s to be compared with `compare`, `>`, `>=`, `<` and `<=`
  -- | whenever there are `Ord` instances for both component types. To obtain
  -- | the result, the `fst`s are `compare`d, and if they are `EQ`ual, the
  -- | `snd`s are `compare`d.
  instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b) where
    compare (Tuple a1 b1) (Tuple a2 b2) = case compare a1 a2 of
      EQ -> compare b1 b2
      other -> other

  instance semigroupoidTuple :: Semigroupoid Tuple where
    (<<<) (Tuple _ c) (Tuple a _) = Tuple a c

  -- | The `Semigroup` instance enables use of the associative operator `<>` on
  -- | `Tuple`s whenever there are `Semigroup` instances for the component
  -- | types. The `<>` operator is applied pairwise, so:
  -- | ```purescript
  -- | (Tuple a1 b1) <> (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)
  -- | ```
  instance semigroupTuple :: (Semigroup a, Semigroup b) => Semigroup (Tuple a b) where
    (<>) (Tuple a1 b1) (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)

  instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b) where
    mempty = Tuple mempty mempty

  -- | The `Functor` instance allows functions to transform the contents of a
  -- | `Tuple` with the `<$>` operator, applying the function to the second
  -- | component, so:
  -- | ```purescript
  -- | f <$> (Tuple x y) = Tuple x (f y)
  -- | ````
  instance functorTuple :: Functor (Tuple a) where
    (<$>) f (Tuple x y) = Tuple x (f y)

  -- | The `Functor` instance allows functions to transform the contents of a
  -- | `Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
  -- | for the `fst` component, so:
  -- | ```purescript
  -- | (Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
  -- | ```
  instance applyTuple :: (Semigroup a) => Apply (Tuple a) where
    (<*>) (Tuple a1 f) (Tuple a2 x) = Tuple (a1 <> a2) (f x)

  instance applicativeTuple :: (Monoid a) => Applicative (Tuple a) where
    pure = Tuple mempty

  instance bindTuple :: (Semigroup a) => Bind (Tuple a) where
    (>>=) (Tuple a1 b) f = case f b of
      Tuple a2 c -> Tuple (a1 <> a2) c

  instance monadTuple :: (Monoid a) => Monad (Tuple a)

  instance extendTuple :: Extend (Tuple a) where
    (<<=) f t@(Tuple a b) = Tuple a (f t)

  instance comonadTuple :: Comonad (Tuple a) where
    extract = snd

  instance lazyTuple :: (Lazy a, Lazy b) => Lazy (Tuple a b) where
    defer f = Tuple (defer $ \_ -> fst (f unit)) (defer $ \_ -> snd (f unit))

  instance lazyLazy1Tuple :: (Lazy1 l1, Lazy1 l2) => Lazy (Tuple (l1 a) (l2 b)) where
    defer f = Tuple (defer1 $ \_ -> fst (f unit)) (defer1 $ \_ -> snd (f unit))

  instance lazyLazy2Tuple :: (Lazy2 l1, Lazy2 l2) => Lazy (Tuple (l1 a b) (l2 c d)) where
    defer f = Tuple (defer2 $ \_ -> fst (f unit)) (defer2 $ \_ -> snd (f unit))

  -- | Returns the first component of a tuple.
  fst :: forall a b. Tuple a b -> a
  fst (Tuple a _) = a

  -- | Returns the second component of a tuple.
  snd :: forall a b. Tuple a b -> b
  snd (Tuple _ b) = b

  -- | Turn a function that expects a tuple into a function of two arguments.
  curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
  curry f a b = f (Tuple a b)

  -- | Turn a function of two arguments into a function that expects a tuple.
  uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
  uncurry f (Tuple a b) = f a b

  -- | Rakes two lists and returns a list of corresponding pairs.
  -- | If one input list is short, excess elements of the longer list are discarded.
  zip :: forall a b. [a] -> [b] -> [Tuple a b]
  zip = zipWith Tuple

  -- | Transforms a list of pairs into a list of first components and a list of
  -- | second components.
  unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
  unzip ((Tuple a b):ts) = case unzip ts of
    Tuple as bs -> Tuple (a : as) (b : bs)
  unzip [] = Tuple [] []

  -- | Exchange the first and second components of a tuple.
  swap :: forall a b. Tuple a b -> Tuple b a
  swap (Tuple a b) = Tuple b a


-- | Utilities for n-tuples: sequences longer than two components built from
-- | nested pairs.
-- |
-- | Nested tuples arise naturally in product combinators. You shouldn't 
-- | represent data using nested tuples, but if combinators you're working with
-- | create them, utilities in this module will allow to to more easily work
-- | with them, including translating to and from more traditional product types.
-- | 
-- | ```purescript
-- | data Address = Address String City (Maybe Province) Country
-- |
-- | exampleAddress1 = makeAddress "221B Baker Street" London Nothing UK
-- | exampleAddress2 = makeAddressT $ "221B Baker Street" /\ London /\ Nothing /\ UK
-- | 
-- | makeAddressT :: Tuple4 String City (Maybe Province) Country -> Address
-- | makeAddressT = uncurry4 Address
-- |
-- | makeAddress :: String -> City -> (Maybe Province) -> Country -> Address
-- | makeAddress = curry4 makeAddressT
-- | 
-- | tupleAddress :: Address -> Tuple4 String City (Maybe Province) Country
-- | tupleAddress (Address a b c d) = tuple4 a b c d
-- | ```
module Data.Tuple.Nested where
  import Data.Tuple

  type Tuple2 a z = Tuple a z
  type Tuple3 a b z = Tuple (Tuple2 a b) z
  type Tuple4 a b c z = Tuple (Tuple3 a b c) z
  type Tuple5 a b c d z = Tuple (Tuple4 a b c d) z
  type Tuple6 a b c d e z = Tuple (Tuple5 a b c d e) z
  type Tuple7 a b c d e f z = Tuple (Tuple6 a b c d e f) z
  type Tuple8 a b c d e f g z = Tuple (Tuple7 a b c d e f g) z
  type Tuple9 a b c d e f g h z = Tuple (Tuple8 a b c d e f g h) z
  type Tuple10 a b c d e f g h i z = Tuple (Tuple9 a b c d e f g h i) z

  -- | Given 2 values, creates a nested 2-tuple.
  tuple2 :: forall a b. a -> b -> Tuple2 a b
  tuple2 = Tuple

  -- | Given 3 values, creates a nested 3-tuple.
  tuple3 :: forall a b c. a -> b -> c -> Tuple3 a b c
  tuple3 a b c = Tuple (Tuple a b) c

  -- | Given 4 values, creates a nested 4-tuple.
  tuple4 :: forall a b c d. a -> b -> c -> d -> Tuple4 a b c d
  tuple4 a b c d = Tuple (Tuple (Tuple a b) c) d

  -- | Given 5 values, creates a nested 5-tuple.
  tuple5 :: forall a b c d e. a -> b -> c -> d -> e -> Tuple5 a b c d e
  tuple5 a b c d e = Tuple (Tuple (Tuple (Tuple a b) c) d) e

  -- | Given 6 values, creates a nested 6-tuple.
  tuple6 :: forall a b c d e f. a -> b -> c -> d -> e -> f -> Tuple6 a b c d e f
  tuple6 a b c d e f = Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f

  -- | Given 7 values, creates a nested 7-tuple.
  tuple7 :: forall a b c d e f g. a -> b -> c -> d -> e -> f -> g -> Tuple7 a b c d e f g
  tuple7 a b c d e f g = Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g

  -- | Given 8 values, creates a nested 8-tuple.
  tuple8 :: forall a b c d e f g h. a -> b -> c -> d -> e -> f -> g -> h -> Tuple8 a b c d e f g h
  tuple8 a b c d e f g h = Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h

  -- | Given 9 values, creates a nested 9-tuple.
  tuple9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> Tuple9 a b c d e f g h i
  tuple9 a b c d e f g h i = Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h) i

  -- | Given 10 values, creates a nested 10-tuple.
  tuple10 :: forall a b c d e f g h i j. a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Tuple10 a b c d e f g h i j
  tuple10 a b c d e f g h i j = Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h) i) j
  
  -- | Given a function of 2 arguments, return a function that accepts a 2-tuple.
  uncurry2 :: forall a b z. (a -> b -> z) -> Tuple2 a b -> z
  uncurry2 f = \(Tuple t z) -> f t z

  -- | Given a function that accepts a 2-tuple, return a function of 2 arguments.
  curry2 :: forall a b z. (Tuple2 a b -> z) -> a -> b -> z
  curry2 f a b = f (Tuple a b)

  -- | Given a function of 3 arguments, return a function that accepts a 3-tuple.
  uncurry3 :: forall a b c z. (a -> b -> c -> z) -> Tuple3 a b c -> z
  uncurry3 f = \(Tuple (Tuple a b) c) -> f a b c

  -- | Given a function that accepts a 3-tuple, return a function of 3 arguments.
  curry3 :: forall a b c z. (Tuple3 a b c -> z) -> a -> b -> c -> z
  curry3 f a b c = f (Tuple (Tuple a b) c)

  -- | Given a function of 4 arguments, return a function that accepts a 4-tuple.
  uncurry4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Tuple4 a b c d -> z
  uncurry4 f = \(Tuple (Tuple (Tuple a b) c) d) -> f a b c d

  -- | Given a function that accepts a 4-tuple, return a function of 4 arguments.
  curry4 :: forall a b c d z. (Tuple4 a b c d -> z) -> a -> b -> c -> d -> z
  curry4 f a b c d = f (Tuple (Tuple (Tuple a b) c) d)

  -- | Given a function of 5 arguments, return a function that accepts a 5-tuple.
  uncurry5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Tuple5 a b c d e -> z
  uncurry5 f = \(Tuple (Tuple (Tuple (Tuple a b) c) d) e) -> f a b c d e

  -- | Given a function that accepts a 5-tuple, return a function of 5 arguments.
  curry5 :: forall a b c d e z. (Tuple5 a b c d e -> z) -> a -> b -> c -> d -> e -> z
  curry5 f a b c d e = f (Tuple (Tuple (Tuple (Tuple a b) c) d) e)

  -- | Given a function of 6 arguments, return a function that accepts a 6-tuple.
  uncurry6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> Tuple6 a b c d e f -> z
  uncurry6 f' = \(Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) -> f' a b c d e f

  -- | Given a function that accepts a 6-tuple, return a function of 6 arguments.
  curry6 :: forall a b c d e f z. (Tuple6 a b c d e f -> z) -> a -> b -> c -> d -> e -> f -> z
  curry6 f' a b c d e f = f' (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f)

  -- | Given a function of 7 arguments, return a function that accepts a 7-tuple.
  uncurry7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> Tuple7 a b c d e f g -> z
  uncurry7 f' = \(Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) -> f' a b c d e f g

  -- | Given a function that accepts a 7-tuple, return a function of 7 arguments.
  curry7 :: forall a b c d e f g z. (Tuple7 a b c d e f g -> z) -> a -> b -> c -> d -> e -> f -> g -> z
  curry7 f' a b c d e f g = f' (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g)

  -- | Given a function of 8 arguments, return a function that accepts a 8-tuple.
  uncurry8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> Tuple8 a b c d e f g h -> z
  uncurry8 f' = \(Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h) -> f' a b c d e f g h

  -- | Given a function that accepts a 8-tuple, return a function of 8 arguments.
  curry8 :: forall a b c d e f g h z. (Tuple8 a b c d e f g h -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> z
  curry8 f' a b c d e f g h = f' (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h)

  -- | Given a function of 9 arguments, return a function that accepts a 9-tuple.
  uncurry9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> Tuple9 a b c d e f g h i -> z
  uncurry9 f' = \(Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h) i) -> f' a b c d e f g h i

  -- | Given a function that accepts a 9-tuple, return a function of 9 arguments.
  curry9 :: forall a b c d e f g h i z. (Tuple9 a b c d e f g h i -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> z
  curry9 f' a b c d e f g h i = f' (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h) i)

  -- | Given a function of 10 arguments, return a function that accepts a 10-tuple.
  uncurry10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> Tuple10 a b c d e f g h i j -> z
  uncurry10 f' = \(Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h) i) j) -> f' a b c d e f g h i j

  -- | Given a function that accepts a 10-tuple, return a function of 10 arguments.
  curry10 :: forall a b c d e f g h i j z. (Tuple10 a b c d e f g h i j -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z
  curry10 f' a b c d e f g h i j = f' (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple (Tuple a b) c) d) e) f) g) h) i) j)

  infixl 6 /\

  -- | Shorthand for constructing n-tuples as nested pairs.
  -- | `a /\ b /\ c /\ d` becomes `Tuple (Tuple (Tuple a b) c ) d`
  (/\) :: forall a b. a -> b -> Tuple a b
  (/\) a b = Tuple a b
