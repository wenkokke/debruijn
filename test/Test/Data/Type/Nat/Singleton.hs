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
      testProperty "test_Unsafe_fromSomeSNat_eq_fromSomeSNatRaw" test_Unsafe_fromSomeSNat_eq_fromSomeSNatRaw
    , testProperty "test_Unsafe_toSomeSNat_eq_toSomeSNatRaw" test_Unsafe_toSomeSNat_eq_toSomeSNatRaw
    , testProperty "test_Unsafe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id" test_Unsafe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id
    , testProperty "test_Unsafe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id" test_Unsafe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id
    , -- Test conversion to/from numbers of Safe API
      testProperty "test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw" test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw
    , testProperty "test_Inductive_toSomeSNat_eq_toSomeSNatRaw" test_Inductive_toSomeSNat_eq_toSomeSNatRaw
    , testProperty "test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id" test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id
    , testProperty "test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id" test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id
    ]

--------------------------------------------------------------------------------
-- Test correspondence between Fast and Safe APIs
--------------------------------------------------------------------------------

test_fromSNatRawEq :: Safe.SomeSNat -> Bool
test_fromSNatRawEq (Safe.SomeSNat n) =
  Safe.fromSNatRaw n == Fast.fromSNatRaw (Fast.fromInductive n)

test_fromSNatEq :: Safe.SomeSNat -> Bool
test_fromSNatEq (Safe.SomeSNat n) =
  Safe.fromSNat @Int n == Fast.fromSNat @Int (Fast.fromInductive n)

test_decSNatEq :: Safe.SomeSNat -> Safe.SomeSNat -> Bool
test_decSNatEq (Safe.SomeSNat m) (Safe.SomeSNat n) =
  Safe.decSNat m n == Fast.decSNat (Fast.fromInductive m) (Fast.fromInductive n)

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Fast API
--------------------------------------------------------------------------------

test_Unsafe_fromSomeSNat_eq_fromSomeSNatRaw :: Fast.SomeSNat -> Bool
test_Unsafe_fromSomeSNat_eq_fromSomeSNatRaw n =
  Fast.fromSomeSNat n == Fast.fromSomeSNatRaw n

test_Unsafe_toSomeSNat_eq_toSomeSNatRaw :: NonNegative Int -> Bool
test_Unsafe_toSomeSNat_eq_toSomeSNatRaw (NonNegative u) =
  Fast.toSomeSNat u == Fast.toSomeSNatRaw u

test_Unsafe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: Fast.SomeSNat -> Bool
test_Unsafe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n =
  Fast.toSomeSNatRaw (Fast.fromSomeSNatRaw n) == n

test_Unsafe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative Int -> Bool
test_Unsafe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative u) =
  Fast.fromSomeSNatRaw (Fast.toSomeSNatRaw u) == u

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Safe API
--------------------------------------------------------------------------------

test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw :: Safe.SomeSNat -> Bool
test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw n =
  Safe.fromSomeSNat n == Safe.fromSomeSNatRaw n

test_Inductive_toSomeSNat_eq_toSomeSNatRaw :: NonNegative Int -> Bool
test_Inductive_toSomeSNat_eq_toSomeSNatRaw (NonNegative u) =
  Safe.toSomeSNat u == Safe.toSomeSNatRaw u

test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: Safe.SomeSNat -> Bool
test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n =
  Safe.toSomeSNatRaw (Safe.fromSomeSNatRaw n) == n

test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative Int -> Bool
test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative u) =
  Safe.fromSomeSNatRaw (Safe.toSomeSNatRaw u) == u
