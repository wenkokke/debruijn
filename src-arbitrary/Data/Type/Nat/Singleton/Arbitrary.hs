{-# LANGUAGE DerivingStrategies #-}

module Data.Type.Nat.Singleton.Arbitrary (SomeSNatRep (..), getSNatRep) where

import Data.Type.Nat.Singleton.Fast (SNatRep)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Modifiers (NonNegative (getNonNegative))

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeSNat
--------------------------------------------------------------------------------

newtype SomeSNatRep = SomeSNatRep SNatRep
  deriving stock (Eq, Show)

getSNatRep :: SomeSNatRep -> SNatRep
getSNatRep (SomeSNatRep nRep) = nRep

instance Arbitrary SomeSNatRep where
  arbitrary :: Gen SomeSNatRep
  arbitrary = SomeSNatRep . getNonNegative <$> arbitrary

  shrink :: SomeSNatRep -> [SomeSNatRep]
  shrink (SomeSNatRep nRep) = SomeSNatRep <$> shrink nRep
