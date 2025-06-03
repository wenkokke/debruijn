{-# LANGUAGE DerivingStrategies #-}

module Data.DeBruijn.Index.Arbitrary (
  SomeIxRep (..),
) where

import Data.DeBruijn.Index.Fast (IxRep, snatRepToIxRep)
import Data.Type.Nat.Singleton.Fast (SNatRep)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Extra (chooseSizedBoundedIntegral)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Modifiers (Positive (..))

data SomeIxRep = SomeIxRep !SNatRep !IxRep
  deriving stock (Eq, Show)

instance Arbitrary SomeIxRep where
  arbitrary :: Gen SomeIxRep
  arbitrary = do
    nRep <- getPositive <$> arbitrary
    iRep <- chooseSizedBoundedIntegral (0, snatRepToIxRep (nRep - 1))
    pure $ SomeIxRep nRep iRep
