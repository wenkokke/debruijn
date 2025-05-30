{-# LANGUAGE DerivingStrategies #-}

module Data.DeBruijn.Thinning.Arbitrary (
  SomeThRep (..),
) where

import Data.Bits (Bits (..))
import Data.DeBruijn.Thinning.Fast (ThRep)
import Data.Type.Nat.Singleton.Fast (SNatRep)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, chooseInteger)
import Test.QuickCheck.Modifiers (NonNegative (..))
import Text.Printf (printf)

data SomeThRep = SomeThRep !SNatRep !ThRep
  deriving stock (Eq)

instance Show SomeThRep where
  showsPrec p (SomeThRep nRep nmRep) =
    let mRep = nRep + popCount nmRep
    in  showParen (p > 10) . showString $
          printf ("SomeThRep %d 0b%0" <> show mRep <> "b") nRep nmRep

instance Arbitrary SomeThRep where
  arbitrary :: Gen SomeThRep
  arbitrary = do
    NonNegative mRep <- arbitrary
    nmRep <- chooseInteger (0, 2 ^ mRep - 1)
    let nRep = mRep - popCount nmRep
    pure $ SomeThRep nRep nmRep
