{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.DeBruijn.Thinning.Arbitrary (
  SomeThRep (..),
  SomeThBoundRep (..),
  getSomeThBoundRep,
) where

import Control.Exception (assert)
import Data.Bits (Bits (..))
import Data.DeBruijn.Thinning.Fast (ThRep, bitsToThRep, thRepToBits)
import Data.Type.Nat.Singleton.Fast (SNatRep, intToSNatRep)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Extra (chooseSizedBoundedIntegral, chooseSizesBoundedPositiveIntegral)
import Test.QuickCheck.Gen (Gen, chooseInteger)
import Test.QuickCheck.Modifiers (NonNegative (..), Positive (..))
import Text.Printf (printf)

data SomeThRep = SomeThRep !SNatRep !ThRep
  deriving stock (Eq)

instance Show SomeThRep where
  showsPrec :: Int -> SomeThRep -> ShowS
  showsPrec p (SomeThRep nRep nmRep) =
    let mRep = nRep + intToSNatRep (popCount nmRep)
    in  showParen (p > 10) . showString $
          printf ("SomeThRep %d 0b%0" <> show mRep <> "b") nRep (thRepToBits @Integer nmRep)

instance Arbitrary SomeThRep where
  arbitrary :: Gen SomeThRep
  arbitrary = do
    mRep <- maybe (getPositive <$> arbitrary) chooseSizesBoundedPositiveIntegral (bitSizeMaybe (undefined :: ThRep))
    nmRep <- bitsToThRep <$> chooseInteger (0, 2 ^ mRep - 1)
    let nRep = mRep - intToSNatRep (popCount nmRep)
    assert (nRep >= 0) $ pure ()
    pure $ SomeThRep nRep nmRep

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeThBoundRep

newtype SomeThBoundRep = SomeThBoundRep SNatRep
  deriving stock (Eq, Show)

getSomeThBoundRep :: SomeThBoundRep -> SNatRep
getSomeThBoundRep (SomeThBoundRep nRep) = nRep

instance Arbitrary SomeThBoundRep where
  arbitrary :: Gen SomeThBoundRep
  arbitrary = case bitSizeMaybe (undefined :: ThRep) of
    Nothing -> SomeThBoundRep . getNonNegative <$> arbitrary
    Just thSize -> SomeThBoundRep <$> chooseSizedBoundedIntegral (0, thSize)

  shrink :: SomeThBoundRep -> [SomeThBoundRep]
  shrink (SomeThBoundRep nRep) = SomeThBoundRep <$> shrink nRep
