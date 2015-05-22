-- | This module defines a type of maps as balanced 2-3 trees, based on
-- | <http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>

module Data.Map
  ( Map(),
    showTree,
    empty,
    isEmpty,
    singleton,
    checkValid,
    insert,
    lookup,
    toList,
    fromList,
    fromListWith,
    delete,
    member,
    alter,
    update,
    keys,
    values,
    union,
    unionWith,
    unions,
    map,
    size
  ) where

import qualified Data.Array as A
import Data.Maybe
import Data.Tuple
import Data.Monoid (Monoid)
import Data.Foldable (foldl, foldMap, foldr, Foldable)
import Data.Traversable (traverse, Traversable)

-- | `Map k v` represents maps from keys of type `k` to values of type `v`.
data Map k v
  = Leaf
  | Two (Map k v) k v (Map k v)
  | Three (Map k v) k v (Map k v) k v (Map k v)

instance eqMap :: (Eq k, Eq v) => Eq (Map k v) where
  (==) m1 m2 = toList m1 == toList m2
  (/=) m1 m2 = not (m1 == m2)

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show m = "fromList " ++ show (toList m)

instance ordMap :: (Ord k, Ord v) => Ord (Map k v) where
  compare m1 m2 = compare (toList m1) (toList m2)

instance semigroupMap :: (Ord k) => Semigroup (Map k v) where
  (<>) = union

instance monoidMap :: (Ord k) => Monoid (Map k v) where
  mempty = empty

instance functorMap :: Functor (Map k) where
  (<$>) _ Leaf = Leaf
  (<$>) f (Two left k v right) = Two (f <$> left) k (f v) (f <$> right)
  (<$>) f (Three left k1 v1 mid k2 v2 right) = Three (f <$> left) k1 (f v1) (f <$> mid) k2 (f v2) (f <$> right)

instance foldableMap :: Foldable (Map k) where
  foldl   f z m = foldl   f z (values m)
  foldr   f z m = foldr   f z (values m)
  foldMap f   m = foldMap f   (values m)

instance traversableMap :: (Ord k) => Traversable (Map k) where
  traverse f ms = foldr (\x acc -> union <$> x <*> acc) (pure empty) (((<$>) (uncurry singleton)) <$> (traverse f <$> toList ms))
  sequence = traverse id

-- | Render a `Map` as a `String`
showTree :: forall k v. (Show k, Show v) => Map k v -> String
showTree Leaf = "Leaf"
showTree (Two left k v right) =
  "Two (" ++ showTree left ++
  ") (" ++ show k ++
  ") (" ++ show v ++
  ") (" ++ showTree right ++ ")"
showTree (Three left k1 v1 mid k2 v2 right) =
  "Three (" ++ showTree left ++
  ") (" ++ show k1 ++
  ") (" ++ show v1 ++
  ") (" ++ showTree mid ++
  ") (" ++ show k2 ++
  ") (" ++ show v2 ++
  ") (" ++ showTree right ++ ")"

-- | An empty map
empty :: forall k v. Map k v
empty = Leaf

-- | Test if a map is empty
isEmpty :: forall k v. Map k v -> Boolean
isEmpty Leaf = true
isEmpty _ = false

-- | Create a map with one key/value pair
singleton :: forall k v. k -> v -> Map k v
singleton k v = Two Leaf k v Leaf

-- | Check whether the underlying tree satisfies the 2-3 invariant
-- |
-- | This function is provided for internal use.
checkValid :: forall k v. Map k v -> Boolean
checkValid tree = A.length (A.nub (allHeights tree)) == 1
  where
  allHeights :: forall k v. Map k v -> [Number]
  allHeights Leaf = [0]
  allHeights (Two left _ _ right) = A.map (\n -> n + 1) (allHeights left ++ allHeights right)
  allHeights (Three left _ _ mid _ _ right) = A.map (\n -> n + 1) (allHeights left ++ allHeights mid ++ allHeights right)

-- | Lookup a value for the specified key
lookup :: forall k v. (Ord k) => k -> Map k v -> Maybe v
lookup _ Leaf = Nothing
lookup k (Two _ k1 v _) | k == k1 = Just v
lookup k (Two left k1 _ _) | k < k1 = lookup k left
lookup k (Two _ _ _ right) = lookup k right
lookup k (Three _ k1 v1 _ _ _ _) | k == k1 = Just v1
lookup k (Three _ _ _ _ k2 v2 _) | k == k2 = Just v2
lookup k (Three left k1 _ _ _ _ _) | k < k1 = lookup k left
lookup k (Three _ k1 _ mid k2 _ _) | k1 < k && k <= k2 = lookup k mid
lookup k (Three _ _ _ _ _ _ right) = lookup k right

-- | Test if a key is a member of a map
member :: forall k v. (Ord k) => k -> Map k v -> Boolean
member k m = isJust (k `lookup` m)

data TreeContext k v
  = TwoLeft k v (Map k v)
  | TwoRight (Map k v) k v
  | ThreeLeft k v (Map k v) k v (Map k v)
  | ThreeMiddle (Map k v) k v k v (Map k v)
  | ThreeRight (Map k v) k v (Map k v) k v

fromZipper :: forall k v. (Ord k) => [TreeContext k v] -> Map k v -> Map k v
fromZipper [] tree = tree
fromZipper (TwoLeft k1 v1 right : ctx) left = fromZipper ctx (Two left k1 v1 right)
fromZipper (TwoRight left k1 v1 : ctx) right = fromZipper ctx (Two left k1 v1 right)
fromZipper (ThreeLeft k1 v1 mid k2 v2 right : ctx) left = fromZipper ctx (Three left k1 v1 mid k2 v2 right)
fromZipper (ThreeMiddle left k1 v1 k2 v2 right : ctx) mid = fromZipper ctx (Three left k1 v1 mid k2 v2 right)
fromZipper (ThreeRight left k1 v1 mid k2 v2 : ctx) right = fromZipper ctx (Three left k1 v1 mid k2 v2 right)

data KickUp k v = KickUp (Map k v) k v (Map k v)

-- | Insert a key/value pair into a map
insert :: forall k v. (Ord k) => k -> v -> Map k v -> Map k v
insert = down []
  where
  down :: forall k v. (Ord k) => [TreeContext k v] -> k -> v -> Map k v -> Map k v
  down ctx k v Leaf = up ctx (KickUp Leaf k v Leaf)
  down ctx k v (Two left k1 _ right) | k == k1 = fromZipper ctx (Two left k v right)
  down ctx k v (Two left k1 v1 right) | k < k1 = down (TwoLeft k1 v1 right : ctx) k v left
  down ctx k v (Two left k1 v1 right) = down (TwoRight left k1 v1 : ctx) k v right
  down ctx k v (Three left k1 _ mid k2 v2 right) | k == k1 = fromZipper ctx (Three left k v mid k2 v2 right)
  down ctx k v (Three left k1 v1 mid k2 _ right) | k == k2 = fromZipper ctx (Three left k1 v1 mid k v right)
  down ctx k v (Three left k1 v1 mid k2 v2 right) | k < k1 = down (ThreeLeft k1 v1 mid k2 v2 right : ctx) k v left
  down ctx k v (Three left k1 v1 mid k2 v2 right) | k1 < k && k <= k2 = down (ThreeMiddle left k1 v1 k2 v2 right : ctx) k v mid
  down ctx k v (Three left k1 v1 mid k2 v2 right) = down (ThreeRight left k1 v1 mid k2 v2 : ctx) k v right

  up :: forall k v. (Ord k) => [TreeContext k v] -> KickUp k v -> Map k v
  up [] (KickUp left k v right) = Two left k v right
  up (TwoLeft k1 v1 right : ctx) (KickUp left k v mid) = fromZipper ctx (Three left k v mid k1 v1 right)
  up (TwoRight left k1 v1 : ctx) (KickUp mid k v right) = fromZipper ctx (Three left k1 v1 mid k v right)
  up (ThreeLeft k1 v1 c k2 v2 d : ctx) (KickUp a k v b) = up ctx (KickUp (Two a k v b) k1 v1 (Two c k2 v2 d))
  up (ThreeMiddle a k1 v1 k2 v2 d : ctx) (KickUp b k v c) = up ctx (KickUp (Two a k1 v1 b) k v (Two c k2 v2 d))
  up (ThreeRight a k1 v1 b k2 v2 : ctx) (KickUp c k v d) = up ctx (KickUp (Two a k1 v1 b) k2 v2 (Two c k v d))

-- | Delete a key and its corresponding value from a map
delete :: forall k v. (Ord k) => k -> Map k v -> Map k v
delete = down []
  where
  down :: forall k v. (Ord k) => [TreeContext k v] -> k -> Map k v -> Map k v
  down ctx _ Leaf = fromZipper ctx Leaf
  down ctx k (Two Leaf k1 _ Leaf) 
    | k == k1 = up ctx Leaf
  down ctx k (Two left k1 v1 right) 
    | k == k1   = let max = maxNode left
                    in removeMaxNode (TwoLeft max.key max.value right : ctx) left
    | k < k1    = down (TwoLeft k1 v1 right : ctx) k left
    | otherwise = down (TwoRight left k1 v1 : ctx) k right
  down ctx k (Three Leaf k1 v1 Leaf k2 v2 Leaf) 
    | k == k1 = fromZipper ctx (Two Leaf k2 v2 Leaf)
    | k == k2 = fromZipper ctx (Two Leaf k1 v1 Leaf)
  down ctx k (Three left k1 v1 mid k2 v2 right) 
    | k == k1 = let max = maxNode left
                  in removeMaxNode (ThreeLeft max.key max.value mid k2 v2 right : ctx) left
    | k == k2 = let max = maxNode mid
                  in removeMaxNode (ThreeMiddle left k1 v1 max.key max.value right : ctx) mid
    | k < k1               = down (ThreeLeft k1 v1 mid k2 v2 right : ctx) k left
    | k1 < k && k < k2 = down (ThreeMiddle left k1 v1 k2 v2 right : ctx) k mid
    | otherwise            = down (ThreeRight left k1 v1 mid k2 v2 : ctx) k right

  up :: forall k v. (Ord k) => [TreeContext k v] -> Map k v -> Map k v
  up [] tree = tree
  up (TwoLeft k1 v1 Leaf : ctx) Leaf = fromZipper ctx (Two Leaf k1 v1 Leaf)
  up (TwoRight Leaf k1 v1 : ctx) Leaf = fromZipper ctx (Two Leaf k1 v1 Leaf)
  up (TwoLeft k1 v1 (Two m k2 v2 r) : ctx) l = up ctx (Three l k1 v1 m k2 v2 r)
  up (TwoRight (Two l k1 v1 m) k2 v2 : ctx) r = up ctx (Three l k1 v1 m k2 v2 r)
  up (TwoLeft k1 v1 (Three b k2 v2 c k3 v3 d) : ctx) a = fromZipper ctx (Two (Two a k1 v1 b) k2 v2 (Two c k3 v3 d))
  up (TwoRight (Three a k1 v1 b k2 v2 c) k3 v3 : ctx) d = fromZipper ctx (Two (Two a k1 v1 b) k2 v2 (Two c k3 v3 d))
  up (ThreeLeft k1 v1 Leaf k2 v2 Leaf : ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (ThreeMiddle Leaf k1 v1 k2 v2 Leaf : ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (ThreeRight Leaf k1 v1 Leaf k2 v2 : ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (ThreeLeft k1 v1 (Two b k2 v2 c) k3 v3 d : ctx) a = fromZipper ctx (Two (Three a k1 v1 b k2 v2 c) k3 v3 d)
  up (ThreeMiddle (Two a k1 v1 b) k2 v2 k3 v3 d : ctx) c = fromZipper ctx (Two (Three a k1 v1 b k2 v2 c) k3 v3 d)
  up (ThreeMiddle a k1 v1 k2 v2 (Two c k3 v3 d) : ctx) b = fromZipper ctx (Two a k1 v1 (Three b k2 v2 c k3 v3 d))
  up (ThreeRight a k1 v1 (Two b k2 v2 c) k3 v3 : ctx) d = fromZipper ctx (Two a k1 v1 (Three b k2 v2 c k3 v3 d))
  up (ThreeLeft k1 v1 (Three b k2 v2 c k3 v3 d) k4 v4 e : ctx) a = fromZipper ctx (Three (Two a k1 v1 b) k2 v2 (Two c k3 v3 d) k4 v4 e)
  up (ThreeMiddle (Three a k1 v1 b k2 v2 c) k3 v3 k4 v4 e : ctx) d = fromZipper ctx (Three (Two a k1 v1 b) k2 v2 (Two c k3 v3 d) k4 v4 e)
  up (ThreeMiddle a k1 v1 k2 v2 (Three c k3 v3 d k4 v4 e) : ctx) b = fromZipper ctx (Three a k1 v1 (Two b k2 v2 c) k3 v3 (Two d k4 v4 e))
  up (ThreeRight a k1 v1 (Three b k2 v2 c k3 v3 d) k4 v4 : ctx) e = fromZipper ctx (Three a k1 v1 (Two b k2 v2 c) k3 v3 (Two d k4 v4 e))

  maxNode :: forall k v. (Ord k) => Map k v -> { key :: k, value :: v }
  maxNode (Two _ k v Leaf) = { key: k, value: v }
  maxNode (Two _ _ _ right) = maxNode right
  maxNode (Three _ _ _ _ k v Leaf) = { key: k, value: v }
  maxNode (Three _ _ _ _ _ _ right) = maxNode right

  removeMaxNode :: forall k v. (Ord k) => [TreeContext k v] -> Map k v -> Map k v
  removeMaxNode ctx (Two Leaf _ _ Leaf) = up ctx Leaf
  removeMaxNode ctx (Two left k v right) = removeMaxNode (TwoRight left k v : ctx) right
  removeMaxNode ctx (Three Leaf k1 v1 Leaf _ _ Leaf) = up (TwoRight Leaf k1 v1 : ctx) Leaf
  removeMaxNode ctx (Three left k1 v1 mid k2 v2 right) = removeMaxNode (ThreeRight left k1 v1 mid k2 v2 : ctx) right

-- | Insert the value, delete a value, or update a value for a key in a map
alter :: forall k v. (Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Update or delete the value for a key in a map
update :: forall k v. (Ord k) => (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = alter (maybe Nothing f) k m

-- | Convert a map to an array of key/value pairs
toList :: forall k v. Map k v -> [Tuple k v]
toList Leaf = []
toList (Two left k v right) = toList left ++ [Tuple k v] ++ toList right
toList (Three left k1 v1 mid k2 v2 right) = toList left ++ [Tuple k1 v1] ++ toList mid ++ [Tuple k2 v2] ++ toList right

-- | Create a map from an array of key/value pairs
fromList :: forall k v. (Ord k) => [Tuple k v] -> Map k v
fromList = foldl (\m (Tuple k v) -> insert k v m) empty

-- | Create a map from an array of key/value pairs, using the specified function
-- | to combine values for duplicate keys.
fromListWith :: forall k v. (Ord k) => (v -> v -> v) -> [Tuple k v] -> Map k v
fromListWith f = foldl (\m (Tuple k v) -> alter (combine v) k m) empty where
  combine v (Just v') = Just $ f v v'
  combine v Nothing = Just v

-- | Get an array of the keys contained in a map
keys :: forall k v. Map k v -> [k]
keys Leaf = []
keys (Two left k _ right) = keys left ++ [k] ++ keys right
keys (Three left k1 _ mid k2 _ right) = keys left ++ [k1] ++ keys mid ++ [k2] ++ keys right

-- | Get an array of the values contained in a map
values :: forall k v. Map k v -> [v]
values Leaf = []
values (Two left _ v right) = values left ++ [v] ++ values right
values (Three left _ v1 mid _ v2 right) = values left ++ [v1] ++ values mid ++ [v2] ++ values right

-- | Compute the union of two maps, using the specified function
-- | to combine values for duplicate keys.
unionWith :: forall k v. (Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f m1 m2 = foldl go m2 (toList m1)
  where
  go m (Tuple k v) = alter (Just <<< maybe v (f v)) k m

-- | Compute the union of two maps, preferring values from the first map in the case
-- | of duplicate keys
union :: forall k v. (Ord k) => Map k v -> Map k v -> Map k v
union = unionWith const

-- | Compute the union of a collection of maps
unions :: forall k v. (Ord k) => [Map k v] -> Map k v
unions = foldl union empty

-- | Apply a function to the values in a map
map :: forall k a b. (a -> b) -> Map k a -> Map k b
map = (<$>)

-- | Calculate the number of key/value pairs in a map
size :: forall k v. Map k v -> Number
size = A.length <<< values
