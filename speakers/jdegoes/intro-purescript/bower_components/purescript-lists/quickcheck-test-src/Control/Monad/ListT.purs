module Test.Control.Monad.ListT where

  import Test.QuickCheck
  import Test.QuickCheck.Gen

  import Control.Monad.ListT

  instance arbitraryListT :: (Monad f, Arbitrary a) => Arbitrary (ListT f a) where
  arbitrary = fromArray <$> arbitrary
