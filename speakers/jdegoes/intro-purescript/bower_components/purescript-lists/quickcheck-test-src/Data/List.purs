module Test.Data.List where

  import Test.QuickCheck
  import Test.QuickCheck.Gen

  import Data.List

  instance arbitraryList :: (Arbitrary a) => Arbitrary (List a) where
  arbitrary = fromArray <$> arbitrary
