{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Test.Data.DeBruijn.Thinning (
  tests,
) where

import Data.Bits (Bits (..))
import Data.DeBruijn.Index.Safe qualified as Fast.Ix (fromInductive, toInductive)
import Data.DeBruijn.Thinning.Arbitrary (SomeThRep (..))
import Data.DeBruijn.Thinning.Fast (ThRep)
import Data.DeBruijn.Thinning.Fast qualified as Fast
import Data.DeBruijn.Thinning.Fast.Arbitrary ()
import Data.DeBruijn.Thinning.Safe qualified as Fast (fromInductive, toInductive)
import Data.DeBruijn.Thinning.Safe qualified as Safe
import Data.DeBruijn.Thinning.Safe.Arbitrary (SomeThickIxArgs (..), SomeThinIxArgs (..), SomeThinThArgs (..))
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat.Singleton.Fast qualified as SNat.Fast
import Data.Type.Nat.Singleton.Safe (fromSNat)
import Data.Type.Nat.Singleton.Safe qualified as SNat.Fast (fromInductive, toInductive)
import Data.Type.Nat.Singleton.Safe qualified as Safe (SNat (..), SomeSNat (..), decSNat, fromSNatRaw)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, counterexample, once, testProperty, (==>))
import Text.Printf (printf)

tests :: TestTree
tests =
  testGroup
    "Test.Data.DeBruijn.Thinning"
    [ -- Test correspondence between Fast and Safe APIs
      testProperty "test_KeepAllTh" test_KeepAllTh
    , testProperty "test_KeepOneTh" test_KeepOneTh
    , testProperty "test_DropOneTh" test_DropOneTh
    , testProperty "test_dropAllEq" test_dropAllEq
    , testProperty "test_toBoolsEq" test_toBoolsEq
    , testProperty "test_thinIxEq" test_thinIxEq
    , testProperty "test_thickIxEq" test_thickIxEq
    , testProperty "test_thinThEq" test_thinThEq
    , testProperty "test_thinTh_eq_thinThFast" test_thinTh_eq_thinThFast
    , testProperty "test_fromSomeThEq" test_fromSomeThEq
    , testProperty "test_fromSomeThRawEq" test_fromSomeThRawEq
    , testProperty "test_toSomeThEq" test_toSomeThEq
    , testProperty "test_toSomeThRawEq" test_toSomeThRawEq
    , -- Test conversion to/from numbers of Fast API
      testProperty "test_Fast_fromSomeTh_eq_fromSomeThRaw" test_Fast_fromSomeTh_eq_fromSomeThRaw
    , testProperty "test_Fast_toSomeTh_eq_toSomeThRaw" test_Fast_toSomeTh_eq_toSomeThRaw
    , testProperty "test_Fast_toSomeThRaw_o_fromSomeThRaw_eq_id" test_Fast_toSomeThRaw_o_fromSomeThRaw_eq_id
    , testProperty "test_Fast_fromSomeThRaw_o_toSomeThRaw_eq_id" test_Fast_fromSomeThRaw_o_toSomeThRaw_eq_id
    , -- Test conversion to/from numbers of Safe API
      testProperty "test_Safe_fromSomeTh_eq_fromSomeThRaw" test_Safe_fromSomeTh_eq_fromSomeThRaw
    , testProperty "test_Safe_toSomeTh_eq_toSomeThRaw" test_Safe_toSomeTh_eq_toSomeThRaw
    , testProperty "test_Safe_toSomeThRaw_o_fromSomeThRaw_eq_id" test_Safe_toSomeThRaw_o_fromSomeThRaw_eq_id
    , testProperty "test_Safe_fromSomeThRaw_o_toSomeThRaw_eq_id" test_Safe_fromSomeThRaw_o_toSomeThRaw_eq_id
    ]

--------------------------------------------------------------------------------
-- Test correspondence between Fast and Safe APIs
--------------------------------------------------------------------------------

-- | Test: Constructor @KeepAll@.
test_KeepAllTh :: Property
test_KeepAllTh =
  once $
    Safe.KeepAll == Fast.toInductive Fast.KeepAll

-- | Internal helper. Check if the test is within the safe range of fast thinnings.
withinSafeThRange :: Safe.SNat n -> Bool
withinSafeThRange n = maybe True (fromSNat n <=) (bitSizeMaybe (undefined :: ThRep))

-- | Internal helper. Check if the test is within the safe range of fast thinnings.
withinSafeThGrowRange :: Safe.SNat n -> Bool
withinSafeThGrowRange n = maybe True (fromSNat n <) (bitSizeMaybe (undefined :: ThRep))

-- | Test: Constructor @KeepOne@.
test_KeepOneTh :: Safe.SomeTh -> Property
test_KeepOneTh (Safe.SomeTh _n m nm) = do
  let expect = Safe.KeepOne nm
  let actual = Fast.toInductive (Fast.KeepOne (Fast.fromInductive nm))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    withinSafeThGrowRange m ==>
      expect
        == actual

-- | Test: Constructor @DropOne@.
test_DropOneTh :: Safe.SomeTh -> Property
test_DropOneTh (Safe.SomeTh _n m nm) = do
  let expect = Safe.DropOne nm
  let actual = Fast.toInductive (Fast.DropOne (Fast.fromInductive nm))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    withinSafeThGrowRange m ==>
      expect
        == actual

-- TODO: Case analysis.

-- | Test: @dropAll@.
test_dropAllEq :: Safe.SomeSNat -> Property
test_dropAllEq (Safe.SomeSNat n) = do
  let expect = Safe.dropAll n
  let actual = Fast.toInductive (Fast.dropAll (SNat.Fast.fromInductive n))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    withinSafeThRange n ==>
      expect
        == actual

-- | Test: @toBools@.
test_toBoolsEq :: Safe.SomeTh -> Property
test_toBoolsEq (Safe.SomeTh _n _m nm) = do
  let expect = Safe.toBools nm
  let actual = Fast.toBools (Fast.fromInductive nm)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @thin@ from instance for indexes.
test_thinIxEq :: SomeThinIxArgs -> Property
test_thinIxEq (SomeThinIxArgs _n m nm i) = do
  let expect = Safe.thin nm i
  let actual = Fast.Ix.toInductive (Fast.thin (Fast.fromInductive nm) (Fast.Ix.fromInductive i))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    withinSafeThRange m ==>
      expect
        == actual

-- | Test: @thick@ from instance for indexes.
test_thickIxEq :: SomeThickIxArgs -> Property
test_thickIxEq (SomeThickIxArgs _n m nm i) = do
  let expect = Safe.thick nm i
  let actual = Fast.Ix.toInductive <$> Fast.thick (Fast.fromInductive nm) (Fast.Ix.fromInductive i)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    withinSafeThRange m ==>
      expect
        == actual

-- | Test: @thin@ from instance for thinnings.
test_thinThEq :: SomeThinThArgs -> Property
test_thinThEq (SomeThinThArgs l n m nm ln) = do
  let expect = Safe.thin nm ln
  let actual = Fast.toInductive (Fast.thin (Fast.fromInductive nm) (Fast.fromInductive ln))
  counterexample (showCase l n m nm ln expect actual) $
    expect == actual

-- | Test: @thin@ from instance for thinnings.
test_thinTh_eq_thinThFast :: SomeThinThArgs -> Property
test_thinTh_eq_thinThFast (SomeThinThArgs l n m nm ln) =
  SNat.Fast.withKnownNat (SNat.Fast.fromInductive m) $ do
    let expect = Fast.thin (Fast.fromInductive nm) (Fast.fromInductive ln)
    let actual = Fast.thinThFast (Fast.fromInductive nm) (Fast.fromInductive ln)
    counterexample (showCase l n m nm ln (Fast.toInductive expect) (Fast.toInductive actual)) $
      expect == actual

showCase :: Safe.SNat l -> Safe.SNat n -> Safe.SNat m -> n Safe.:<= m -> l Safe.:<= n -> l Safe.:<= m -> l Safe.:<= m -> String
showCase l n m nm ln expect actual =
  printf
    "%02d<=%02d: %s\n%02d<=%02d: %s\nexpect: %s\nactual: %s"
    (Safe.fromSNatRaw n)
    (Safe.fromSNatRaw m)
    (showBits nm)
    (Safe.fromSNatRaw l)
    (Safe.fromSNatRaw n)
    (showBits ln)
    (showBits expect)
    (showBits actual)
 where
  showBits :: n Safe.:<= m -> String
  showBits = printf "0b%064b" . Safe.fromTh @Natural

-- TODO: @thick@ from instance for thinnings.
-- This test is incredibly annoying to write, because its inputs are two
-- thinnings @n :< m@ and @l :< m@, which cannot be obtained with the current
-- operations on type-level natural numbers.

-- | Test: @fromSomeTh@.
test_fromSomeThEq :: Safe.SomeTh -> Property
test_fromSomeThEq (Safe.SomeTh n m nm) = do
  let expect = Safe.fromSomeTh @Int @Integer (Safe.SomeTh n m nm)
  let actual = Fast.fromSomeTh (Fast.SomeTh (SNat.Fast.fromInductive n) (SNat.Fast.fromInductive m) (Fast.fromInductive nm))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromSomeThRaw@.
test_fromSomeThRawEq :: Safe.SomeTh -> Property
test_fromSomeThRawEq (Safe.SomeTh n m nm) = do
  let expect = Safe.fromSomeThRaw (Safe.SomeTh n m nm)
  let actual = Fast.fromSomeThRaw (Fast.SomeTh (SNat.Fast.fromInductive n) (SNat.Fast.fromInductive m) (Fast.fromInductive nm))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @toSomeTh@.
test_toSomeThEq :: SomeThRep -> Property
test_toSomeThEq (SomeThRep nRep nmRep) = do
  let expect = Safe.toSomeTh (nRep, nmRep)
  let actual = Fast.toSomeTh (nRep, nmRep)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    case (expect, actual) of
      (Safe.SomeTh n1 m1 nm1, Fast.SomeTh n2 m2 nm2) ->
        case (n1 `Safe.decSNat` SNat.Fast.toInductive n2, m1 `Safe.decSNat` SNat.Fast.toInductive m2) of
          (Just Refl, Just Refl) -> nm1 == Fast.toInductive nm2
          _otherwise -> False

-- | Test: @toSomeThRaw@.
test_toSomeThRawEq :: SomeThRep -> Property
test_toSomeThRawEq (SomeThRep nRep nmRep) = do
  let expect = Safe.toSomeThRaw (nRep, nmRep)
  let actual = Fast.toSomeThRaw (nRep, nmRep)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    case (expect, actual) of
      (Safe.SomeTh n1 m1 nm1, Fast.SomeTh n2 m2 nm2) ->
        case (n1 `Safe.decSNat` SNat.Fast.toInductive n2, m1 `Safe.decSNat` SNat.Fast.toInductive m2) of
          (Just Refl, Just Refl) -> nm1 == Fast.toInductive nm2
          _otherwise -> False

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Fast API
--------------------------------------------------------------------------------

-- | Test: @fromSomeTh == fromSomeThRaw@.
test_Fast_fromSomeTh_eq_fromSomeThRaw :: Fast.SomeTh -> Property
test_Fast_fromSomeTh_eq_fromSomeThRaw nm = do
  let expect = Fast.fromSomeTh nm
  let actual = Fast.fromSomeThRaw nm
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @toSomeTh == toSomeThRaw@.
test_Fast_toSomeTh_eq_toSomeThRaw :: SomeThRep -> Property
test_Fast_toSomeTh_eq_toSomeThRaw (SomeThRep nRep nmRep) = do
  let expect = Fast.toSomeTh (nRep, nmRep)
  let actual = Fast.toSomeThRaw (nRep, nmRep)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @toSomeThRaw . fromSomeThRaw == id@.
test_Fast_toSomeThRaw_o_fromSomeThRaw_eq_id :: Fast.SomeTh -> Property
test_Fast_toSomeThRaw_o_fromSomeThRaw_eq_id nm = do
  let expect = nm
  let actual = Fast.toSomeThRaw (Fast.fromSomeThRaw nm)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromSomeThRaw . toSomeThRaw == id@.
test_Fast_fromSomeThRaw_o_toSomeThRaw_eq_id :: SomeThRep -> Property
test_Fast_fromSomeThRaw_o_toSomeThRaw_eq_id (SomeThRep nRep iRep) = do
  let expect = (nRep, iRep)
  let actual = Fast.fromSomeThRaw (Fast.toSomeThRaw (nRep, iRep))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- Corollary: @toSomeTh . fromSomeTh == id@.

-- Corollary: @fromSomeTh . toSomeTh == id@.

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Safe API
--------------------------------------------------------------------------------

-- | Test: @fromSomeTh == fromSomeThRaw@.
test_Safe_fromSomeTh_eq_fromSomeThRaw :: Safe.SomeTh -> Bool
test_Safe_fromSomeTh_eq_fromSomeThRaw nm =
  Safe.fromSomeTh nm == Safe.fromSomeThRaw nm

-- | Test: @toSomeTh == toSomeThRaw@.
test_Safe_toSomeTh_eq_toSomeThRaw :: SomeThRep -> Bool
test_Safe_toSomeTh_eq_toSomeThRaw (SomeThRep nRep nmRep) =
  Safe.toSomeTh (nRep, nmRep) == Safe.toSomeThRaw (nRep, nmRep)

-- | Test: @toSomeThRaw . fromSomeThRaw == id@.
test_Safe_toSomeThRaw_o_fromSomeThRaw_eq_id :: Safe.SomeTh -> Bool
test_Safe_toSomeThRaw_o_fromSomeThRaw_eq_id nm =
  Safe.toSomeThRaw (Safe.fromSomeThRaw nm) == nm

-- | Test: @fromSomeThRaw . toSomeThRaw == id@.
test_Safe_fromSomeThRaw_o_toSomeThRaw_eq_id :: SomeThRep -> Bool
test_Safe_fromSomeThRaw_o_toSomeThRaw_eq_id (SomeThRep nRep iRep) =
  Safe.fromSomeThRaw (Safe.toSomeThRaw (nRep, iRep)) == (nRep, iRep)

-- Corollary: @toSomeTh . fromSomeTh == id@.

-- Corollary: @fromSomeTh . toSomeTh == id@.
