-- | This module defines the error monad transformer, `ErrorT`.

module Control.Monad.Error.Trans where

import Control.Apply 
import Control.Alt
import Control.Alternative
import Control.Plus
import Control.Monad.Error
import Control.Monad.Trans
import Control.MonadPlus
import Data.Either
import Data.Monoid
import Data.Tuple

-- | The error monad transformer
-- |
-- | This monad transformer extends the base monad with the ability to throw and handle 
-- | errors.
-- |
-- | The `MonadError` type class describes the operations supported by this monad.
newtype ErrorT e m a = ErrorT (m (Either e a))

-- | Run a computation in the `ErrorT` monad.
runErrorT :: forall e m a. ErrorT e m a -> m (Either e a)
runErrorT (ErrorT x) = x

-- | Change the error and result types in an `ErrorT` monad action.
mapErrorT :: forall e1 e2 m1 m2 a b. (m1 (Either e1 a) -> m2 (Either e2 b)) -> ErrorT e1 m1 a -> ErrorT e2 m2 b
mapErrorT f m = ErrorT $ f (runErrorT m)

instance functorErrorT :: (Functor m) => Functor (ErrorT e m) where
  (<$>) f = ErrorT <<< (<$>) ((<$>) f) <<< runErrorT

instance applyErrorT :: (Apply m) => Apply (ErrorT e m) where
  (<*>) (ErrorT f) (ErrorT v) = ErrorT $ lift2 ($) <$> f <*> v

instance applicativeErrorT :: (Applicative m) => Applicative (ErrorT e m) where
  pure a = ErrorT $ pure $ Right a

instance altErrorT :: (Monad m) => Alt (ErrorT e m) where
  (<|>) x y = ErrorT $ runErrorT x >>= \e -> case e of
    Left _ -> runErrorT y
    r -> return r

instance plusErrorT :: (Monad m, Error e) => Plus (ErrorT e m) where
  empty = ErrorT (return (Left $ strMsg "No alternative"))

instance alternativeErrorT :: (Monad m, Error e) => Alternative (ErrorT e m)

instance bindErrorT :: (Monad m) => Bind (ErrorT e m) where
  (>>=) m f = ErrorT $ do
    a <- runErrorT m
    case a of
      Left e -> return $ Left e
      Right x -> runErrorT (f x)

instance monadErrorT :: (Monad m) => Monad (ErrorT e m)

instance monadPlusErrorT :: (Monad m, Error e) => MonadPlus (ErrorT e m)

instance monadTransErrorT :: MonadTrans (ErrorT e) where
  lift m = ErrorT $ do
    a <- m
    return $ Right a

liftListenError :: forall e m a w. (Monad m) => (m (Either e a) -> m (Tuple (Either e a) w)) -> ErrorT e m a -> ErrorT e m (Tuple a w)
liftListenError listen = mapErrorT $ \m -> do
  Tuple a w <- listen m
  return $ (\r -> Tuple r w) <$> a

liftPassError :: forall e m a w. (Monad m) => (m (Tuple (Either e a) (w -> w)) -> m (Either e a)) -> ErrorT e m (Tuple a (w -> w)) -> ErrorT e m a
liftPassError pass = mapErrorT $ \m -> pass $ do
  a <- m
  return $ case a of
    Left e -> Tuple (Left e) id
    Right (Tuple r f) -> Tuple (Right r) f

liftCallCCError :: forall e m a b. (((Either e a -> m (Either e b)) -> m (Either e a)) -> m (Either e a)) -> ((a -> ErrorT e m b) -> ErrorT e m a) -> ErrorT e m a
liftCallCCError callCC f = ErrorT $ callCC $ \c -> runErrorT (f (\a -> ErrorT $ c (Right a)))
