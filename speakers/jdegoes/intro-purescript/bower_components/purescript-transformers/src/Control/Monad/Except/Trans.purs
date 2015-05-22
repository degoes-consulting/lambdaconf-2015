
module Control.Monad.Except.Trans where

import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Data.Either
import Data.Monoid

-- | A monad transformer which adds exceptions to other monads, in the same way
-- | as `Except`. As before, `e` is the type of exceptions, and `a` is the type
-- | of successful results. The new type parameter `m` is the inner monad that
-- | computations run in.
newtype ExceptT e m a = ExceptT (m (Either e a))

-- | The inverse of `ExceptT`. Run a computation in the `ExceptT` monad.
runExceptT :: forall e m a. ExceptT e m a -> m (Either e a)
runExceptT (ExceptT x) = x

-- | Transform any exceptions thrown by an `ExceptT` computation using the given function.
withExceptT :: forall e e' m a. (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = ExceptT <<< (<$>) (mapLeft f) <<< runExceptT
  where
  mapLeft _ (Right x) = Right x
  mapLeft f (Left x) = Left (f x)

-- | Transform the unwrapped computation using the given function.
mapExceptT :: forall e e' m n a b. (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f m = ExceptT (f (runExceptT m))

instance functorExceptT :: (Functor m) => Functor (ExceptT e m) where
  (<$>) f = mapExceptT ((<$>) ((<$>) f))

instance applyExceptT :: (Apply m) => Apply (ExceptT e m) where
  (<*>) (ExceptT f) (ExceptT x) =
    let f' = (<*>) <$> f
        x' = f' <*> x
    in ExceptT x'

instance applicativeExceptT :: (Applicative m) => Applicative (ExceptT e m) where
  pure = ExceptT <<< pure <<< Right

instance bindExceptT :: (Monad m) => Bind (ExceptT e m) where
  (>>=) m k = ExceptT (runExceptT m >>=
                          either (return <<< Left) (runExceptT <<< k))

instance monadExceptT :: (Monad m) => Monad (ExceptT e m)

instance altExceptT :: (Semigroup e, Monad m) => Alt (ExceptT e m) where
  (<|>) m n = ExceptT $ do
    rm <- runExceptT m
    case rm of
      Right x -> pure (Right x)
      Left err -> do
        rn <- runExceptT n
        case rn of
          Right x -> pure (Right x)
          Left err' -> pure (Left (err <> err'))

instance plusExceptT :: (Monoid e, Monad m) => Plus (ExceptT e m) where
  empty = throwE mempty

instance alternativeExceptT :: (Monoid e, Monad m) => Alternative (ExceptT e m)

instance monadPlusExceptT :: (Monoid e, Monad m) => MonadPlus (ExceptT e m)

-- | Throw an exception in an `ExceptT` computation.
throwE :: forall e m a. (Applicative m) => e -> ExceptT e m a
throwE = ExceptT <<< pure <<< Left

-- | Catch an exception in an `ExceptT` computation.
catchE :: forall e e' m a. (Monad m) => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
catchE m handler = ExceptT (runExceptT m >>= either (runExceptT <<< handler) (pure <<< Right))
