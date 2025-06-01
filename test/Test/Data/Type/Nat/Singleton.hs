module Test.Data.Type.Nat.Singleton (tests) where

import Data.Maybe (isJust)
import Data.Type.Nat.Singleton.Fast qualified as Fast
import Data.Type.Nat.Singleton.Fast.Arbitrary ()
import Data.Type.Nat.Singleton.Safe (SNatRep)
import Data.Type.Nat.Singleton.Safe qualified as Fast (fromInductive, toInductive)
import Data.Type.Nat.Singleton.Safe qualified as Safe
import Data.Type.Nat.Singleton.Safe.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonNegative (..), Property, counterexample, testProperty)
import Text.Printf (printf)

tests :: TestTree
tests =
  testGroup
    "Test.Data.Type.Nat.Singleton"
    [ -- Test correspondence between Fast and Safe APIs
      testProperty "test_fromSNatRawEq" test_fromSNatRawEq
    , testProperty "test_fromSNatEq" test_fromSNatEq
    , testProperty "test_decSNatEq" test_decSNatEq
    , testProperty "test_fromSomeSNatEq" test_fromSomeSNatEq
    , testProperty "test_fromSomeSNatRawEq" test_fromSomeSNatRawEq
    , testProperty "test_toSomeSNatEq" test_toSomeSNatEq
    , testProperty "test_toSomeSNatRawEq" test_toSomeSNatRawEq
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
test_fromSNatRawEq :: Safe.SomeSNat -> Property
test_fromSNatRawEq (Safe.SomeSNat n) = do
  let expect = Safe.fromSNatRaw n
  let actual = Fast.fromSNatRaw (Fast.fromInductive n)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromSNat@.
test_fromSNatEq :: Safe.SomeSNat -> Property
test_fromSNatEq (Safe.SomeSNat n) = do
  let expect = Safe.fromSNat @Int n
  let actual = Fast.fromSNat @Int (Fast.fromInductive n)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- TODO: @plus@.

-- | Test: @decSNat@.
test_decSNatEq :: Safe.SomeSNat -> Safe.SomeSNat -> Property
test_decSNatEq (Safe.SomeSNat m) (Safe.SomeSNat n) = do
  let expect = Safe.decSNat m n
  let actual = Fast.decSNat (Fast.fromInductive m) (Fast.fromInductive n)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromSomeSNat@.
test_fromSomeSNatEq :: Safe.SomeSNat -> Property
test_fromSomeSNatEq (Safe.SomeSNat n) = do
  let expect = Safe.fromSomeSNat @Int (Safe.SomeSNat n)
  let actual = Fast.fromSomeSNat (Fast.SomeSNat (Fast.fromInductive n))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromSomeSNatRaw@.
test_fromSomeSNatRawEq :: Safe.SomeSNat -> Property
test_fromSomeSNatRawEq (Safe.SomeSNat n) = do
  let expect = Safe.fromSomeSNatRaw (Safe.SomeSNat n)
  let actual = Fast.fromSomeSNatRaw (Fast.SomeSNat (Fast.fromInductive n))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @toSomeSNat@.
test_toSomeSNatEq :: NonNegative Safe.SNatRep -> Property
test_toSomeSNatEq (NonNegative nRep) = do
  let expect = Safe.toSomeSNat nRep
  let actual = Fast.toSomeSNat nRep
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    case (expect, actual) of
      (Safe.SomeSNat n1, Fast.SomeSNat n2) ->
        isJust (n1 `Safe.decSNat` Fast.toInductive n2)

-- | Test: @toSomeSNatRaw@.
test_toSomeSNatRawEq :: NonNegative Safe.SNatRep -> Property
test_toSomeSNatRawEq (NonNegative nRep) = do
  let expect = Safe.toSomeSNatRaw nRep
  let actual = Fast.toSomeSNatRaw nRep
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    case (expect, actual) of
      (Safe.SomeSNat n1, Fast.SomeSNat n2) ->
        isJust (n1 `Safe.decSNat` Fast.toInductive n2)

-- TODO: @withKnownSNat@.

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Fast API
--------------------------------------------------------------------------------

-- | Test: @fromSomeSNat == fromSomeSNatRaw@.
test_Fast_fromSomeSNat_eq_fromSomeSNatRaw :: Fast.SomeSNat -> Property
test_Fast_fromSomeSNat_eq_fromSomeSNatRaw n = do
  let expect = Fast.fromSomeSNat n
  let actual = Fast.fromSomeSNatRaw n
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @toSomeSNat == toSomeSNatRaw@.
test_Fast_toSomeSNat_eq_toSomeSNatRaw :: NonNegative SNatRep -> Property
test_Fast_toSomeSNat_eq_toSomeSNatRaw (NonNegative nRep) = do
  let expect = Fast.toSomeSNat nRep
  let actual = Fast.toSomeSNatRaw nRep
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @toSomeSNatRaw . fromSomeSNatRaw == id@.
test_Fast_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: Fast.SomeSNat -> Property
test_Fast_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n = do
  let expect = n
  let actual = Fast.toSomeSNatRaw (Fast.fromSomeSNatRaw n)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromSomeSNatRaw . toSomeSNatRaw == id@.
test_Fast_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative SNatRep -> Property
test_Fast_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative nRep) = do
  let expect = nRep
  let actual = Fast.fromSomeSNatRaw (Fast.toSomeSNatRaw nRep)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- Corollary: @toSomeSNat . fromSomeSNat == id@.

-- Corollary: @fromSomeSNat . toSomeSNat == id@.

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Safe API
--------------------------------------------------------------------------------

-- | Test: @fromSomeSNat == fromSomeSNatRaw@.
test_Safe_fromSomeSNat_eq_fromSomeSNatRaw :: Safe.SomeSNat -> Property
test_Safe_fromSomeSNat_eq_fromSomeSNatRaw n = do
  let expect = Safe.fromSomeSNat n
  let actual = Safe.fromSomeSNatRaw n
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @toSomeSNat == toSomeSNatRaw@.
test_Safe_toSomeSNat_eq_toSomeSNatRaw :: NonNegative SNatRep -> Property
test_Safe_toSomeSNat_eq_toSomeSNatRaw (NonNegative nRep) = do
  let expect = Safe.toSomeSNat nRep
  let actual = Safe.toSomeSNatRaw nRep
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @toSomeSNatRaw . fromSomeSNatRaw == id@.
test_Safe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: Safe.SomeSNat -> Property
test_Safe_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n = do
  let expect = n
  let actual = Safe.toSomeSNatRaw (Safe.fromSomeSNatRaw n)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromSomeSNatRaw . toSomeSNatRaw == id@.
test_Safe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative SNatRep -> Property
test_Safe_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative nRep) = do
  let expect = nRep
  let actual = Safe.fromSomeSNatRaw (Safe.toSomeSNatRaw nRep)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- Corollary: @toSomeSNat . fromSomeSNat == id@.

-- Corollary: @fromSomeSNat . toSomeSNat == id@.
