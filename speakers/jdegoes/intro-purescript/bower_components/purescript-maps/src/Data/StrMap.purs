-- | This module defines a type of native Javascript maps which 
-- | require the keys to be strings.
-- | 
-- | To maximize performance, Javascript objects are not wrapped,
-- | and some native code is used even when it's not necessary.

module Data.StrMap
  ( StrMap(),
    empty,
    isEmpty,
    size,
    singleton,
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
    unions,
    map,
    isSubmap,
    fold,
    foldMap,
    foldM,
    foldMaybe,
    all,

    thawST,
    freezeST,
    runST
  ) where

import Control.Monad.Eff (Eff(), runPure)
import Data.Foldable (Foldable, foldl, foldr, for_)
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Monoid.All
import Data.Tuple
import Data.Traversable (Traversable, traverse)
import qualified Control.Monad.ST as ST
import qualified Data.Array as A
import qualified Data.StrMap.ST as SM

-- | `StrMap a` represents a map from `String`s to values of type `a`.
foreign import data StrMap :: * -> *

foreign import _copy
  """
  function _copy(m) {
    var r = {};
    for (var k in m) {
      r[k] = m[k];
    }
    return r;
  }
  """ :: forall a. StrMap a -> StrMap a

foreign import _copyEff
  """
  function _copyEff(m) {
    return function() {
      var r = {};
      for (var k in m) {
        r[k] = m[k];
      }
      return r;
    };
  }
  """ :: forall a b h r. a -> Eff (st :: ST.ST h | r) b

-- | Convert an immutable map into a mutable map
thawST :: forall a h r. StrMap a -> Eff (st :: ST.ST h | r) (SM.STStrMap h a)
thawST = _copyEff

-- | Convert a mutable map into an immutable map
freezeST :: forall a h r. SM.STStrMap h a -> Eff (st :: ST.ST h | r) (StrMap a)
freezeST = _copyEff

-- | Freeze a mutable map, creating an immutable map. Use this function as you would use 
-- | `Prelude.runST` to freeze a mutable reference.
-- | 
-- | The rank-2 type prevents the map from escaping the scope of `runST`.
foreign import runST
  """
  function runST(f) {
    return f;
  }
  """ :: forall a r. (forall h. Eff (st :: ST.ST h | r) (SM.STStrMap h a)) -> Eff r (StrMap a)

pureST :: forall a b. (forall h e. Eff (st :: ST.ST h | e) (SM.STStrMap h a)) -> StrMap a
pureST f = runPure (runST f)

mutate :: forall a b. (forall h e. SM.STStrMap h a -> Eff (st :: ST.ST h | e) b) -> StrMap a -> StrMap a
mutate f m = pureST (do
  s <- thawST m
  f s
  return s)

foreign import _fmapStrMap
  """
  function _fmapStrMap(m0, f) {
    var m = {};
    for (var k in m0) {
      m[k] = f(m0[k]);
    }
    return m;
  }
  """ :: forall a b. Fn2 (StrMap a) (a -> b) (StrMap b)

instance functorStrMap :: Functor StrMap where
  (<$>) f m = runFn2 _fmapStrMap m f

foreign import _foldM
  """
  function _foldM(bind) {
    return function(f) {
      return function(mz) {
        return function(m) {
          function g(k) {
            return function (z) {
              return f(z)(k)(m[k]);
            };
          }
          for (var k in m) {
            mz = bind(mz)(g(k));
          }
          return mz;
        };
      };
    };
  }
  """ :: forall a m z. (m -> (z -> m) -> m) -> (z -> String -> a -> m) -> m -> StrMap a -> m

-- | Fold the keys and values of a map
fold :: forall a z. (z -> String -> a -> z) -> z -> StrMap a -> z
fold = _foldM ((#))

-- | Fold the keys and values of a map, accumulating values using
-- | some `Monoid`.
foldMap :: forall a m. (Monoid m) => (String -> a -> m) -> StrMap a -> m
foldMap f = fold (\acc k v -> acc <> f k v) mempty

-- | Fold the keys and values of a map, accumulating values and effects in
-- | some `Monad`.
foldM :: forall a m z. (Monad m) => (z -> String -> a -> m z) -> z -> StrMap a -> m z
foldM f z = _foldM (>>=) f (pure z)

instance foldableStrMap :: Foldable StrMap where
  foldl f = fold (\z _ -> f z)
  foldr f z m = foldr f z (values m)
  foldMap f = foldMap (const f)

instance traversableStrMap :: Traversable StrMap where
  traverse f ms = foldr (\x acc -> union <$> x <*> acc) (pure empty) (((<$>) (uncurry singleton)) <$> (traverse f <$> toList ms))
  sequence = traverse id

-- Unfortunately the above are not short-circuitable (consider using purescript-machines)
-- so we need special cases:

foreign import _foldSCStrMap
  """
  function _foldSCStrMap(m, z, f, fromMaybe) {
    for (var k in m) {
      var maybeR = f(z)(k)(m[k]);
      var r = fromMaybe(null)(maybeR);
      if (r === null) return z;
      else z = r;
    }
    return z;
  }
  """ :: forall a z. Fn4 (StrMap a) z (z -> String -> a -> Maybe z) (forall a. a -> Maybe a -> a) z

-- | Fold the keys and values of a map.
-- |
-- | This function allows the folding function to terminate the fold early,
-- | using `Maybe`.
foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> StrMap a -> z
foldMaybe f z m = runFn4 _foldSCStrMap m z f fromMaybe

-- | Test whether all key/value pairs in a `StrMap` satisfy a predicate.
foreign import all
  """
  function all(f) {
    return function(m) {
      for (var k in m) {
        if (!f(k)(m[k])) return false;
      }
      return true;
    };
  }
  """ :: forall a. (String -> a -> Boolean) -> StrMap a -> Boolean

instance eqStrMap :: (Eq a) => Eq (StrMap a) where
  (==) m1 m2 = (isSubmap m1 m2) && (isSubmap m2 m1)
  (/=) m1 m2 = not (m1 == m2)

instance showStrMap :: (Show a) => Show (StrMap a) where
  show m = "fromList " ++ show (toList m)

-- | An empty map
foreign import empty "var empty = {};" :: forall a. StrMap a

-- | Test whether one map contains all of the keys and values contained in another map
isSubmap :: forall a. (Eq a) => StrMap a -> StrMap a -> Boolean
isSubmap m1 m2 = all f m1 where
  f k v = runFn4 _lookup false ((==) v) k m2

-- | Test whether a map is empty
isEmpty :: forall a. StrMap a -> Boolean
isEmpty = all (\_ _ -> false)

-- | Calculate the number of key/value pairs in a map
foreign import size
  """
  function size(m) {
    var s = 0;
    for (var k in m) {
      ++s;
    }
    return s;
  }
  """ :: forall a. StrMap a -> Number

-- | Create a map with one key/value pair
singleton :: forall a. String -> a -> StrMap a
singleton k v = pureST (do
  s <- SM.new
  SM.poke s k v
  return s)

foreign import _lookup
  """
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  """ :: forall a z. Fn4 z (a -> z) String (StrMap a) z

-- | Lookup the value for a key in a map
lookup :: forall a. String -> StrMap a -> Maybe a
lookup = runFn4 _lookup Nothing Just

-- | Test whether a `String` appears as a key in a map
member :: forall a. String -> StrMap a -> Boolean
member = runFn4 _lookup false (const true)

-- | Insert a key and value into a map
insert :: forall a. String -> a -> StrMap a -> StrMap a
insert k v = mutate (\s -> SM.poke s k v)

foreign import _unsafeDeleteStrMap
  """
  function _unsafeDeleteStrMap(m, k) {
     delete m[k];
     return m;
  }
  """ :: forall a. Fn2 (StrMap a) String (StrMap a)

-- | Delete a key and value from a map
delete :: forall a. String -> StrMap a -> StrMap a
delete k = mutate (\s -> SM.delete s k)

-- | Insert, remove or update a value for a key in a map
alter :: forall a. (Maybe a -> Maybe a) -> String -> StrMap a -> StrMap a
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Remove or update a value for a key in a map
update :: forall a. (a -> Maybe a) -> String -> StrMap a -> StrMap a
update f k m = alter (maybe Nothing f) k m

-- | Create a map from an array of key/value pairs
fromList :: forall a. [Tuple String a] -> StrMap a
fromList l = pureST (do
  s <- SM.new
  for_ l (\(Tuple k v) -> SM.poke s k v)
  return s)

foreign import _lookupST
  """
  function _lookupST(no, yes, k, m) {
    return function() {
      return k in m ? yes(m[k]) : no;
    }
  }
  """ :: forall a h r z. Fn4 z (a -> z) String (SM.STStrMap h a) (Eff (st :: ST.ST h | r) z)

-- | Create a map from an array of key/value pairs, using the specified function
-- | to combine values for duplicate keys.
fromListWith :: forall a. (a -> a -> a) -> [Tuple String a] -> StrMap a
fromListWith f l = pureST (do
  s <- SM.new
  for_ l (\(Tuple k v) -> runFn4 _lookupST v (f v) k s >>= SM.poke s k)
  return s)

foreign import _collect
  """
  function _collect(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        r.push(f(k)(m[k]));
      }
      return r;
    };
  }
  """ :: forall a b . (String -> a -> b) -> StrMap a -> [b]

-- | Convert a map into an array of key/value pairs
toList :: forall a. StrMap a -> [Tuple String a]
toList = _collect Tuple

-- | Get an array of the keys in a map
foreign import keys
  """
  var keys = Object.keys || _collect(function(k) {
    return function() { return k; };
  });
  """ :: forall a. StrMap a -> [String]

-- | Get an array of the values in a map
values :: forall a. StrMap a -> [a]
values = _collect (\_ v -> v)

-- | Compute the union of two maps, preferring the first map in the case of 
-- | duplicate keys.
union :: forall a. StrMap a -> StrMap a -> StrMap a
union m = mutate (\s -> foldM SM.poke s m)

-- | Compute the union of a collection of maps
unions :: forall a. [StrMap a] -> StrMap a
unions = foldl union empty

-- | Map a function over the values in a map
map :: forall a b. (a -> b) -> StrMap a -> StrMap b
map = (<$>)

instance semigroupStrMap :: (Semigroup a) => Semigroup (StrMap a) where
  (<>) m1 m2 = mutate (\s -> foldM (\s k v2 -> SM.poke s k (runFn4 _lookup v2 (\v1 -> v1 <> v2) k m2)) s m1) m2

instance monoidStrMap :: (Semigroup a) => Monoid (StrMap a) where
  mempty = empty
