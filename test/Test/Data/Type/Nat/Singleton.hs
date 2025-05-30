module Test.Data.Type.Nat.Singleton (tests) where

import Data.Type.Nat.Singleton.Fast qualified as Fast
import Data.Type.Nat.Singleton.Fast.Arbitrary ()
import Data.Type.Nat.Singleton.Safe qualified as Fast (fromInductive)
import Data.Type.Nat.Singleton.Safe qualified as Safe
import Data.Type.Nat.Singleton.Safe.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonNegative (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "Test.Data.Type.Nat.Singleton"
    [ -- Test correspondence between Fast and Safe APIs
      testProperty "test_fromSNatRawEq" test_fromSNatRawEq
    , testProperty "test_fromSNatEq" test_fromSNatEq
    , testProperty "test_decSNatEq" test_decSNatEq
    , -- Test conversion to/from numbers of Fast API
      testProperty "test_Fast_fromSomeSNat_eq_fromSomeSNatRaw" test_Fast_fromSomeSNat_eq_fromSomeSNatRaw
    , testProperty "test_Fast_toSomeSNat_eq_toSomeSNatRaw" test_Fast_toSomeSNat_eq_toSomeSNatRaw
    , testProperty "test_Fast_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id" test_Fast_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id
    , testProperty "test_Fast_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id" test_Fast_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id
    , -- Test conversion to/from numbers of Safe API
      testProperty "test_Safe_fromSomeSNat_eq_fromSomeSNatRaw" test_Safe_fromSomeSNat_eq_fromSomeSNatRaw
    , testProperty "test_Safe_toSomeSNat_eq_toSomeSNatRaw" test_Safe_toSomeSNat_eq_toSomeSNatRaw
    , testProperty "test_Safe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id" test_Safe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id
    , testProperty "test_Safe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id" test_Safe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id
    ]

--------------------------------------------------------------------------------
-- Test correspondence between Fast and Safe APIs
--------------------------------------------------------------------------------

-- TODO: Constructor @Z@.
-- TODO: Constructor @S@.
-- TODO: Case analysis.

-- | Test: @fromSNatRaw@.
test_fromSNatRawEq :: Safe.SomeSNat -> Bool
test_fromSNatRawEq (Safe.SomeSNat n) =
  Safe.fromSNatRaw n == Fast.fromSNatRaw (Fast.fromInductive n)

-- | Test: @fromSNat@.
test_fromSNatEq :: Safe.SomeSNat -> Bool
test_fromSNatEq (Safe.SomeSNat n) =
  Safe.fromSNat @Int n == Fast.fromSNat @Int (Fast.fromInductive n)

-- TODO: @plus@.

-- | Test: @decSNat@.
test_decSNatEq :: Safe.SomeSNat -> Safe.SomeSNat -> Bool
test_decSNatEq (Safe.SomeSNat m) (Safe.SomeSNat n) =
  Safe.decSNat m n == Fast.decSNat (Fast.fromInductive m) (Fast.fromInductive n)

-- TODO: @withKnownSNat@.

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Fast API
--------------------------------------------------------------------------------

-- | Test: @fromSomeSNat == fromSomeSNatRaw@.
test_Fast_fromSomeSNat_eq_fromSomeSNatRaw :: Fast.SomeSNat -> Bool
test_Fast_fromSomeSNat_eq_fromSomeSNatRaw n =
  Fast.fromSomeSNat n == Fast.fromSomeSNatRaw n

-- | Test: @toSomeSNat == toSomeSNatRaw@.
test_Fast_toSomeSNat_eq_toSomeSNatRaw :: NonNegative Int -> Bool
test_Fast_toSomeSNat_eq_toSomeSNatRaw (NonNegative nRep) =
  Fast.toSomeSNat nRep == Fast.toSomeSNatRaw nRep

-- | Test: @toSomeSNatRaw . fromSomeSNatRaw == id@.
test_Fast_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: Fast.SomeSNat -> Bool
test_Fast_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n =
  Fast.toSomeSNatRaw (Fast.fromSomeSNatRaw n) == n

-- | Test: @fromSomeSNatRaw . toSomeSNatRaw == id@.
test_Fast_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative Int -> Bool
test_Fast_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative nRep) =
  Fast.fromSomeSNatRaw (Fast.toSomeSNatRaw nRep) == nRep

-- Corollary: @toSomeSNat . fromSomeSNat == id@.

-- Corollary: @fromSomeSNat . toSomeSNat == id@.

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Safe API
--------------------------------------------------------------------------------

-- | Test: @fromSomeSNat == fromSomeSNatRaw@.
test_Safe_fromSomeSNat_eq_fromSomeSNatRaw :: Safe.SomeSNat -> Bool
test_Safe_fromSomeSNat_eq_fromSomeSNatRaw n =
  Safe.fromSomeSNat n == Safe.fromSomeSNatRaw n

-- | Test: @toSomeSNat == toSomeSNatRaw@.
test_Safe_toSomeSNat_eq_toSomeSNatRaw :: NonNegative Int -> Bool
test_Safe_toSomeSNat_eq_toSomeSNatRaw (NonNegative nRep) =
  Safe.toSomeSNat nRep == Safe.toSomeSNatRaw nRep

-- | Test: @toSomeSNatRaw . fromSomeSNatRaw == id@.
test_Safe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: Safe.SomeSNat -> Bool
test_Safe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n =
  Safe.toSomeSNatRaw (Safe.fromSomeSNatRaw n) == n

-- | Test: @fromSomeSNatRaw . toSomeSNatRaw == id@.
test_Safe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative Int -> Bool
test_Safe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative nRep) =
  Safe.fromSomeSNatRaw (Safe.toSomeSNatRaw nRep) == nRep

-- Corollary: @toSomeSNat . fromSomeSNat == id@.

-- Corollary: @fromSomeSNat . toSomeSNat == id@.
