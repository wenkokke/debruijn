{-# LANGUAGE DerivingStrategies #-}

module Data.DeBruijn.Index.Arbitrary (
  SomeIxRep (..),
) where

import Data.DeBruijn.Index.Fast (IxRep)
import Data.Type.Nat.Singleton.Fast (SNatRep)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, chooseBoundedIntegral)
import Test.QuickCheck.Modifiers (Positive (..))

data SomeIxRep = SomeIxRep !SNatRep !IxRep
  deriving stock (Eq, Show)

instance Arbitrary SomeIxRep where
  arbitrary :: Gen SomeIxRep
  arbitrary = do
    Positive nRep <- arbitrary
    iRep <- chooseBoundedIntegral (0, nRep - 1)
    pure $ SomeIxRep nRep iRep
