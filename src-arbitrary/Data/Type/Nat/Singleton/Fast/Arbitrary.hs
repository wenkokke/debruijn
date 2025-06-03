{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Type.Nat.Singleton.Fast.Arbitrary () where

import Data.Type.Nat.Singleton.Arbitrary (getSNatRep)
import Data.Type.Nat.Singleton.Fast (SomeSNat (..), fromSomeSNatRaw, toSomeSNatRaw)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..), shrinkIntegral)
import Test.QuickCheck.Function (Function (..), functionMap, (:->))
import Test.QuickCheck.Gen (Gen)

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeSNat
--------------------------------------------------------------------------------

instance Arbitrary SomeSNat where
  arbitrary :: Gen SomeSNat
  arbitrary = toSomeSNatRaw . getSNatRep <$> arbitrary

  shrink :: SomeSNat -> [SomeSNat]
  shrink = fmap toSomeSNatRaw . shrinkIntegral . fromSomeSNatRaw

instance CoArbitrary SomeSNat where
  coarbitrary :: SomeSNat -> Gen b -> Gen b
  coarbitrary = coarbitrary . fromSomeSNatRaw

instance Function SomeSNat where
  function :: (SomeSNat -> b) -> SomeSNat :-> b
  function = functionMap fromSomeSNatRaw toSomeSNatRaw
