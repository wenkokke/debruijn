{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Test.Data.DeBruijn.Index (tests) where

import Data.Data (type (:~:) (Refl))
import Data.DeBruijn.Index.Arbitrary (SomeIxRep (..))
import Data.DeBruijn.Index.Fast qualified as Fast
import Data.DeBruijn.Index.Fast.Arbitrary ()
import Data.DeBruijn.Index.Safe qualified as Fast (fromInductive, toInductive)
import Data.DeBruijn.Index.Safe qualified as Safe
import Data.DeBruijn.Index.Safe.Arbitrary ()
import Data.Type.Nat.Singleton.Safe (SNat (..))
import Data.Type.Nat.Singleton.Safe qualified as SNat.Fast (fromInductive)
import Data.Type.Nat.Singleton.Safe qualified as SNat.Safe (SomeSNat (..), decSNat)
import Data.Type.Nat.Singleton.Safe.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonNegative (..), Positive (..), Property, counterexample, once, testProperty)
import Text.Printf (printf)

tests :: TestTree
tests =
  testGroup
    "Test.DeBruijn.Data.Index"
    [ -- Test correspondence between Fast and Safe APIs
      testProperty "test_zeroIx" test_zeroIx
    , testProperty "test_succIx" test_succIx
    , testProperty "test_caseIx" test_caseIx
    , testProperty "test_eqIxEq" test_eqIxEq
    , testProperty "test_fromIxRawEq" test_fromIxRawEq
    , testProperty "test_fromIxEq" test_fromIxEq
    , testProperty "test_thinEq" test_thinEq
    , testProperty "test_thickEq" test_thickEq
    , testProperty "test_injectEq" test_injectEq
    , testProperty "test_raiseEq" test_raiseEq
    , -- Test conversion to/from numbers of Fast API
      testProperty "test_Fast_fromSomeIx_eq_fromSomeIxRaw" test_Fast_fromSomeIx_eq_fromSomeIxRaw
    , testProperty "test_Fast_toSomeIx_eq_toSomeIxRaw" test_Fast_toSomeIx_eq_toSomeIxRaw
    , testProperty "test_Fast_toSomeIxRaw_o_fromSomeIxRaw_eq_id" test_Fast_toSomeIxRaw_o_fromSomeIxRaw_eq_id
    , testProperty "test_Fast_fromSomeIxRaw_o_toSomeIxRaw_eq_id" test_Fast_fromSomeIxRaw_o_toSomeIxRaw_eq_id
    , -- Test conversion to/from numbers of Safe API
      testProperty "test_Safe_fromSomeIx_eq_fromSomeIxRaw" test_Safe_fromSomeIx_eq_fromSomeIxRaw
    , testProperty "test_Safe_toSomeIx_eq_toSomeIxRaw" test_Safe_toSomeIx_eq_toSomeIxRaw
    , testProperty "test_Safe_toSomeIxRaw_o_fromSomeIxRaw_eq_id" test_Safe_toSomeIxRaw_o_fromSomeIxRaw_eq_id
    , testProperty "test_Safe_fromSomeIxRaw_o_toSomeIxRaw_eq_id" test_Safe_fromSomeIxRaw_o_toSomeIxRaw_eq_id
    ]

--------------------------------------------------------------------------------
-- Test correspondence between Fast and Safe APIs
--------------------------------------------------------------------------------

-- | Test: Constructor @FZ@.
test_zeroIx :: Property
test_zeroIx =
  once $
    Safe.FZ == Fast.toInductive Fast.FZ

-- | Test: Constructor @FS@.
test_succIx :: Safe.SomeIx -> Property
test_succIx (Safe.SomeIx _ i) = do
  let expect = Safe.FS i
  let actual = Fast.toInductive (Fast.FS (Fast.fromInductive i))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: Pattern matching.
test_caseIx :: Safe.SomeIx -> Bool
test_caseIx (Safe.SomeIx _ i) =
  case (i, Fast.fromInductive i) of
    (Safe.FZ, Fast.FZ) -> True
    (Safe.FS i', Fast.FS j') -> i' == Fast.toInductive j'
    _ -> False

-- | Test: @eqIx@.
test_eqIxEq :: Safe.SomeIx -> Safe.SomeIx -> Property
test_eqIxEq (Safe.SomeIx _ i) (Safe.SomeIx _ j) = do
  let expect = Safe.eqIx i j
  let actual = Fast.eqIx (Fast.fromInductive i) (Fast.fromInductive j)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromIx@.
test_fromIxEq :: Safe.SomeIx -> Property
test_fromIxEq (Safe.SomeIx _ i) = do
  let expect = Safe.fromIx @Int i
  let actual = Fast.fromIx @Int (Fast.fromInductive i)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @fromIxRaw@.
test_fromIxRawEq :: Safe.SomeIx -> Property
test_fromIxRawEq (Safe.SomeIx _ i) = do
  let expect = Safe.fromIxRaw i
  let actual = Fast.fromIxRaw (Fast.fromInductive i)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @thin@.
test_thinEq :: (Positive Int, NonNegative Int, NonNegative Int) -> Property
test_thinEq (Positive dRaw, NonNegative iRaw, NonNegative jRaw)
  | let nRaw = dRaw + (iRaw `max` jRaw)
  , Safe.SomeIx (S n) i <- Safe.toSomeIxRaw (nRaw + 1, iRaw)
  , Safe.SomeIx n' j <- Safe.toSomeIxRaw (nRaw, jRaw)
  , Just Refl <- SNat.Safe.decSNat n n' = do
      let expect = Safe.thin i j
      let actual = Fast.toInductive (Fast.thin (Fast.fromInductive i) (Fast.fromInductive j))
      counterexample (printf "%s == %s" (show expect) (show actual)) $
        expect == actual
  | otherwise = error "test_thinEq: could not construct test"

-- | Test: @thick@.
test_thickEq :: (Positive Int, NonNegative Int, NonNegative Int) -> Property
test_thickEq (Positive dRaw, NonNegative iRaw, NonNegative jRaw)
  | let nRaw = dRaw + (iRaw `max` jRaw)
  , Safe.SomeIx (S n) i <- Safe.toSomeIxRaw (nRaw, iRaw)
  , Safe.SomeIx (S n') j <- Safe.toSomeIxRaw (nRaw, jRaw)
  , Just Refl <- SNat.Safe.decSNat n n' = do
      let expect = Safe.thick i j
      let actual = Fast.toInductive <$> Fast.thick (Fast.fromInductive i) (Fast.fromInductive j)
      counterexample (printf "%s == %s" (show expect) (show actual)) $
        expect == actual
  | otherwise = error "test_thinEq: could not construct test"

-- | Test: @inject@.
test_injectEq :: Safe.SomeIx -> SNat.Safe.SomeSNat -> Property
test_injectEq (Safe.SomeIx _ i) (SNat.Safe.SomeSNat m) = do
  let expect = Safe.inject i m
  let actual = Fast.toInductive (Fast.inject (Fast.fromInductive i) (SNat.Fast.fromInductive m))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- | Test: @raise@.
test_raiseEq :: SNat.Safe.SomeSNat -> Safe.SomeIx -> Property
test_raiseEq (SNat.Safe.SomeSNat n) (Safe.SomeIx _ j) = do
  let expect = Safe.raise n j
  let actual = Fast.toInductive (Fast.raise (SNat.Fast.fromInductive n) (Fast.fromInductive j))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Fast API
--------------------------------------------------------------------------------

test_Fast_fromSomeIx_eq_fromSomeIxRaw :: Fast.SomeIx -> Bool
test_Fast_fromSomeIx_eq_fromSomeIxRaw n =
  Fast.fromSomeIx n == Fast.fromSomeIxRaw n

test_Fast_toSomeIx_eq_toSomeIxRaw :: SomeIxRep -> Bool
test_Fast_toSomeIx_eq_toSomeIxRaw (SomeIxRep nRep iRep) =
  Fast.toSomeIx (nRep, iRep) == Fast.toSomeIxRaw (nRep, iRep)

test_Fast_toSomeIxRaw_o_fromSomeIxRaw_eq_id :: Fast.SomeIx -> Bool
test_Fast_toSomeIxRaw_o_fromSomeIxRaw_eq_id n =
  Fast.toSomeIxRaw (Fast.fromSomeIxRaw n) == n

test_Fast_fromSomeIxRaw_o_toSomeIxRaw_eq_id :: SomeIxRep -> Bool
test_Fast_fromSomeIxRaw_o_toSomeIxRaw_eq_id (SomeIxRep nRep iRep) =
  Fast.fromSomeIxRaw (Fast.toSomeIxRaw (nRep, iRep)) == (nRep, iRep)

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Safe API
--------------------------------------------------------------------------------

test_Safe_fromSomeIx_eq_fromSomeIxRaw :: Safe.SomeIx -> Bool
test_Safe_fromSomeIx_eq_fromSomeIxRaw n =
  Safe.fromSomeIx n == Safe.fromSomeIxRaw n

test_Safe_toSomeIx_eq_toSomeIxRaw :: SomeIxRep -> Bool
test_Safe_toSomeIx_eq_toSomeIxRaw (SomeIxRep nRep iRep) =
  Safe.toSomeIx (nRep, iRep) == Safe.toSomeIxRaw (nRep, iRep)

test_Safe_toSomeIxRaw_o_fromSomeIxRaw_eq_id :: Safe.SomeIx -> Bool
test_Safe_toSomeIxRaw_o_fromSomeIxRaw_eq_id n =
  Safe.toSomeIxRaw (Safe.fromSomeIxRaw n) == n

test_Safe_fromSomeIxRaw_o_toSomeIxRaw_eq_id :: SomeIxRep -> Bool
test_Safe_fromSomeIxRaw_o_toSomeIxRaw_eq_id (SomeIxRep nRep iRep) =
  Safe.fromSomeIxRaw (Safe.toSomeIxRaw (nRep, iRep)) == (nRep, iRep)
