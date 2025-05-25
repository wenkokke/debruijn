{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Test.Data.DeBruijn.Index (tests) where

import Data.Data (type (:~:) (Refl))
import Data.DeBruijn.Index.Fast qualified as Fast
import Data.DeBruijn.Index.Safe qualified as Fast (fromInductive, toInductive)
import Data.DeBruijn.Index.Safe qualified as Safe
import Data.DeBruijn.Index.Safe.Arbitrary ()
import Data.Proxy (Proxy (..))
import Data.Type.Nat.Singleton.Safe (SNat (..))
import Data.Type.Nat.Singleton.Safe qualified as Safe (SomeSNat (..), decSNat)
import Data.Type.Nat.Singleton.Safe.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonNegative (..), Positive (..), Property, counterexample, once, testProperty)
import Text.Printf (printf)

tests :: TestTree
tests =
  testGroup
    "Test.DeBruijn.Data.Index"
    [ testProperty "test_zeroIx" test_zeroIx
    , testProperty "test_succIx" test_succIx
    , testProperty "test_caseIx" test_caseIx
    , testProperty "test_eqIxEq" test_eqIxEq
    , testProperty "test_fromIxRawEq" test_fromIxRawEq
    , testProperty "test_fromIxEq" test_fromIxEq
    , testProperty "test_injectEq" test_injectEq
    , testProperty "test_thinEq" test_thinEq
    , testProperty "test_thickEq" test_thickEq
    ]

test_zeroIx :: Property
test_zeroIx =
  once $
    Safe.FZ == Fast.toInductive Fast.FZ

test_succIx :: Safe.SomeIx -> Property
test_succIx (Safe.SomeIx _ i) = do
  let expect = Safe.FS i
  let actual = Fast.toInductive (Fast.FS (Fast.fromInductive i))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_caseIx :: Safe.SomeIx -> Bool
test_caseIx (Safe.SomeIx _ i) =
  case (i, Fast.fromInductive i) of
    (Safe.FZ, Fast.FZ) -> True
    (Safe.FS i', Fast.FS j') -> i' == Fast.toInductive j'
    _ -> False

test_eqIxEq :: Safe.SomeIx -> Safe.SomeIx -> Property
test_eqIxEq (Safe.SomeIx _ i) (Safe.SomeIx _ j) = do
  let expect = Safe.eqIx i j
  let actual = Fast.eqIx (Fast.fromInductive i) (Fast.fromInductive j)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_fromIxRawEq :: Safe.SomeIx -> Property
test_fromIxRawEq (Safe.SomeIx _ i) = do
  let expect = Safe.fromIxRaw i
  let actual = Fast.fromIxRaw (Fast.fromInductive i)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_fromIxEq :: Safe.SomeIx -> Property
test_fromIxEq (Safe.SomeIx _ i) = do
  let expect = Safe.fromIx @Int i
  let actual = Fast.fromIx @Int (Fast.fromInductive i)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_injectEq :: Safe.SomeSNat -> Safe.SomeIx -> Property
test_injectEq (Safe.SomeSNat n) (Safe.SomeIx _ i) = do
  let expect = Safe.inject n i
  let actual = Fast.toInductive (Fast.inject (erase n) (Fast.fromInductive i))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_thinEq :: (Positive Int, NonNegative Int, NonNegative Int) -> Property
test_thinEq (Positive dRaw, NonNegative iRaw, NonNegative jRaw)
  | let nRaw = dRaw + (iRaw `max` jRaw)
  , Safe.SomeIx (S n) i <- Safe.toSomeIxRaw (nRaw + 1, iRaw)
  , Safe.SomeIx n' j <- Safe.toSomeIxRaw (nRaw, jRaw)
  , Just Refl <- Safe.decSNat n n' = do
      let expect = Safe.thin i j
      let actual = Fast.toInductive (Fast.thin (Fast.fromInductive i) (Fast.fromInductive j))
      counterexample (printf "%s == %s" (show expect) (show actual)) $
        expect == actual
  | otherwise = error "test_thinEq: could not construct test"

test_thickEq :: (Positive Int, NonNegative Int, NonNegative Int) -> Property
test_thickEq (Positive dRaw, NonNegative iRaw, NonNegative jRaw)
  | let nRaw = dRaw + (iRaw `max` jRaw)
  , Safe.SomeIx (S n) i <- Safe.toSomeIxRaw (nRaw, iRaw)
  , Safe.SomeIx (S n') j <- Safe.toSomeIxRaw (nRaw, jRaw)
  , Just Refl <- Safe.decSNat n n' = do
      let expect = Safe.thick i j
      let actual = Fast.toInductive <$> Fast.thick (Fast.fromInductive i) (Fast.fromInductive j)
      counterexample (printf "%s == %s" (show expect) (show actual)) $
        expect == actual
  | otherwise = error "test_thinEq: could not construct test"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | @`erase` x@ erases the content of @x@ to a @`Proxy`@.
erase :: f a -> Proxy a
erase _ = Proxy
{-# INLINE erase #-}
