{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Test.Data.DeBruijn.Index (tests) where

import Data.Data (type (:~:) (Refl))
import Data.DeBruijn.Index.Inductive qualified as Inductive
import Data.DeBruijn.Index.Inductive qualified as Unsafe (fromInductive, toInductive)
import Data.DeBruijn.Index.Inductive.Arbitrary ()
import Data.DeBruijn.Index.Unsafe qualified as Unsafe
import Data.Proxy (Proxy (..))
import Data.Type.Nat.Singleton.Inductive (SNat (..))
import Data.Type.Nat.Singleton.Inductive qualified as Inductive (SomeSNat (..), decSNat)
import Data.Type.Nat.Singleton.Inductive.Arbitrary ()
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
    Inductive.FZ == Unsafe.toInductive Unsafe.FZ

test_succIx :: Inductive.SomeIx -> Property
test_succIx (Inductive.SomeIx _ i) = do
  let expect = Inductive.FS i
  let actual = Unsafe.toInductive (Unsafe.FS (Unsafe.fromInductive i))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_caseIx :: Inductive.SomeIx -> Bool
test_caseIx (Inductive.SomeIx _ i) =
  case (i, Unsafe.fromInductive i) of
    (Inductive.FZ, Unsafe.FZ) -> True
    (Inductive.FS i', Unsafe.FS j') -> i' == Unsafe.toInductive j'
    _ -> False

test_eqIxEq :: Inductive.SomeIx -> Inductive.SomeIx -> Property
test_eqIxEq (Inductive.SomeIx _ i) (Inductive.SomeIx _ j) = do
  let expect = Inductive.eqIx i j
  let actual = Unsafe.eqIx (Unsafe.fromInductive i) (Unsafe.fromInductive j)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_fromIxRawEq :: Inductive.SomeIx -> Property
test_fromIxRawEq (Inductive.SomeIx _ i) = do
  let expect = Inductive.fromIxRaw i
  let actual = Unsafe.fromIxRaw (Unsafe.fromInductive i)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_fromIxEq :: Inductive.SomeIx -> Property
test_fromIxEq (Inductive.SomeIx _ i) = do
  let expect = Inductive.fromIx @Int i
  let actual = Unsafe.fromIx @Int (Unsafe.fromInductive i)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_injectEq :: Inductive.SomeSNat -> Inductive.SomeIx -> Property
test_injectEq (Inductive.SomeSNat n) (Inductive.SomeIx _ i) = do
  let expect = Inductive.inject n i
  let actual = Unsafe.toInductive (Unsafe.inject (erase n) (Unsafe.fromInductive i))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_thinEq :: (Positive Int, NonNegative Int, NonNegative Int) -> Property
test_thinEq (Positive dRaw, NonNegative iRaw, NonNegative jRaw)
  | let nRaw = dRaw + (iRaw `max` jRaw)
  , Inductive.SomeIx (S n) i <- Inductive.toSomeIxRaw (nRaw + 1, iRaw)
  , Inductive.SomeIx n' j <- Inductive.toSomeIxRaw (nRaw, jRaw)
  , Just Refl <- Inductive.decSNat n n' = do
      let expect = Inductive.thin i j
      let actual = Unsafe.toInductive (Unsafe.thin (Unsafe.fromInductive i) (Unsafe.fromInductive j))
      counterexample (printf "%s == %s" (show expect) (show actual)) $
        expect == actual
  | otherwise = error "test_thinEq: could not construct test"

test_thickEq :: (Positive Int, NonNegative Int, NonNegative Int) -> Property
test_thickEq (Positive dRaw, NonNegative iRaw, NonNegative jRaw)
  | let nRaw = dRaw + (iRaw `max` jRaw)
  , Inductive.SomeIx (S n) i <- Inductive.toSomeIxRaw (nRaw, iRaw)
  , Inductive.SomeIx (S n') j <- Inductive.toSomeIxRaw (nRaw, jRaw)
  , Just Refl <- Inductive.decSNat n n' = do
      let expect = Inductive.thick i j
      let actual = Unsafe.toInductive <$> Unsafe.thick (Unsafe.fromInductive i) (Unsafe.fromInductive j)
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
