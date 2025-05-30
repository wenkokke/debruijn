{-# LANGUAGE DerivingStrategies #-}

module Data.DeBruijn.Thinning.Arbitrary (
  SomeThRep (..),
) where

import Data.Bits (Bits (..))
import Data.DeBruijn.Thinning.Fast (ThRep)
import Data.Type.Nat.Singleton.Fast (SNatRep)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, chooseInteger)
import Test.QuickCheck.Modifiers (Positive (..))

data SomeThRep = SomeThRep !SNatRep !SNatRep !ThRep
  deriving stock (Eq, Show)

instance Arbitrary SomeThRep where
  arbitrary :: Gen SomeThRep
  arbitrary = do
    Positive mRep <- arbitrary
    nmRep <- chooseInteger (0, 2 ^ mRep - 1)
    let nRep = popCount nmRep
    pure $ SomeThRep nRep mRep nmRep
