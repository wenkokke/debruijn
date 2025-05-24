{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Test.Data.DeBruijn.Index (tests) where

import Data.Data (type (:~:) (Refl))
import Data.DeBruijn.Index.Inductive (SomeIx (bound, index))
import Data.DeBruijn.Index.Inductive qualified as Inductive
import Data.DeBruijn.Index.Inductive qualified as Unsafe (fromInductive, toInductive)
import Data.DeBruijn.Index.Inductive.Arbitrary ()
import Data.DeBruijn.Index.Unsafe qualified as Unsafe
import Data.Proxy (Proxy (..))
import Data.Type.Nat.Singleton.Inductive (SNat (..))
import Data.Type.Nat.Singleton.Inductive qualified as Inductive (SomeSNat (..), decSNat)
import Data.Type.Nat.Singleton.Inductive.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonNegative (..), Positive (..), testProperty)

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

test_zeroIx :: Bool
test_zeroIx =
  Inductive.FZ == Unsafe.toInductive Unsafe.FZ

test_succIx :: Inductive.SomeIx -> Bool
test_succIx (Inductive.SomeIx _n i) =
  Inductive.FS i == Unsafe.toInductive (Unsafe.FS (Unsafe.fromInductive i))

test_caseIx :: Inductive.SomeIx -> Bool
test_caseIx (Inductive.SomeIx _n i) =
  case (i, Unsafe.fromInductive i) of
    (Inductive.FZ, Unsafe.FZ) -> True
    (Inductive.FS i', Unsafe.FS j') -> i' == Unsafe.toInductive j'
    _ -> False

test_eqIxEq :: Inductive.SomeIx -> Inductive.SomeIx -> Bool
test_eqIxEq (Inductive.SomeIx{index = i, bound = _}) (Inductive.SomeIx{index = j, bound = _}) =
  Inductive.eqIx i j == Unsafe.eqIx (Unsafe.fromInductive i) (Unsafe.fromInductive j)

test_fromIxRawEq :: Inductive.SomeIx -> Bool
test_fromIxRawEq (Inductive.SomeIx{index = i, bound = _}) =
  Inductive.fromIxRaw i == Unsafe.fromIxRaw (Unsafe.fromInductive i)

test_fromIxEq :: Inductive.SomeIx -> Bool
test_fromIxEq (Inductive.SomeIx{index = i, bound = _}) =
  Inductive.fromIx @Int i == Unsafe.fromIx @Int (Unsafe.fromInductive i)

test_injectEq :: Inductive.SomeSNat -> Inductive.SomeIx -> Bool
test_injectEq (Inductive.SomeSNat n) (Inductive.SomeIx{index = i, bound = _}) =
  Inductive.inject n i == Unsafe.toInductive (Unsafe.inject (erase n) (Unsafe.fromInductive i))

test_thinEq :: (Positive Int, NonNegative Int, NonNegative Int) -> Bool
test_thinEq (Positive dRaw, NonNegative iRaw, NonNegative jRaw)
  | let nRaw = dRaw + (iRaw `max` jRaw)
  , Inductive.SomeIx (S n) i <- Inductive.toSomeIxRaw (nRaw + 1, iRaw)
  , Inductive.SomeIx n' j <- Inductive.toSomeIxRaw (nRaw, jRaw)
  , Just Refl <- Inductive.decSNat n n' =
      Inductive.thin i j == Unsafe.toInductive (Unsafe.thin (Unsafe.fromInductive i) (Unsafe.fromInductive j))
  | otherwise = error "test_thinEq: could not construct test"

test_thickEq :: (Positive Int, NonNegative Int, NonNegative Int) -> Bool
test_thickEq (Positive dRaw, NonNegative iRaw, NonNegative jRaw)
  | let nRaw = dRaw + (iRaw `max` jRaw)
  , Inductive.SomeIx (S n) i <- Inductive.toSomeIxRaw (nRaw, iRaw)
  , Inductive.SomeIx (S n') j <- Inductive.toSomeIxRaw (nRaw, jRaw)
  , Just Refl <- Inductive.decSNat n n' =
      Inductive.thick i j == (Unsafe.toInductive <$> Unsafe.thick (Unsafe.fromInductive i) (Unsafe.fromInductive j))
  | otherwise = error "test_thinEq: could not construct test"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | @`erase` x@ erases the content of @x@ to a @`Proxy`@.
erase :: f a -> Proxy a
erase _ = Proxy
{-# INLINE erase #-}
