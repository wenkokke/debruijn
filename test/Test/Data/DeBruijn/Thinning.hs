{-# LANGUAGE RecordWildCards #-}

module Test.Data.DeBruijn.Thinning
  ( tests,
  )
where

import Data.DeBruijn.Thinning.Unsafe qualified as Unsafe
import Data.DeBruijn.Thinning.Inductive qualified as Inductive
import Data.DeBruijn.Thinning.Inductive (Thin(..))
import Data.Type.Nat.Singleton.Inductive qualified as Inductive.Nat
import Data.DeBruijn.Thinning.Inductive.Arbitrary qualified as Inductive (arbitraryTh)
import Data.DeBruijn.Index.Inductive qualified as Inductive.Ix (fromInductive, toInductive)
import Data.DeBruijn.Index.Inductive.Arbitrary qualified as Inductive (arbitraryIx)
import Data.Type.Nat.Singleton.Unsafe qualified as Unsafe.Nat
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary (arbitrary), Gen)
import Data.Type.Nat.Singleton.Inductive.Arbitrary ()


tests :: TestTree
tests =
  testGroup
    "Test.Data.DeBruijn.Thinning"
    [ testProperty "test_keepAllEq" test_keepAllEq
    , testProperty "test_dropAllEq" test_dropAllEq
    , testProperty "test_toBools" test_toBools
    -- , testProperty "test_InstThinIxEq" test_InstThinIxEq
    ]

test_keepAllEq :: Inductive.Nat.SomeSNat -> Bool
test_keepAllEq (Inductive.Nat.SomeSNat n) =
  Inductive.keepAll n == Inductive.toInductive (Unsafe.keepAll (Inductive.Nat.fromInductive n))

test_dropAllEq :: Inductive.Nat.SomeSNat -> Bool
test_dropAllEq (Inductive.Nat.SomeSNat n) =
  Inductive.dropAll n == Inductive.toInductive (Unsafe.dropAll (Inductive.Nat.fromInductive n))

test_toBools :: Gen Bool
test_toBools = do
  (bools :: [Bool]) <- arbitrary
  pure (roundTrip bools)
  where
    roundTrip :: [Bool] -> Bool
    roundTrip bools
      | Inductive.SomeTh { value = v1 } <- Inductive.fromBools bools,
        Unsafe.SomeTh { value = v2 } <- Unsafe.fromBools bools
      = Inductive.toBools v1 == bools && Unsafe.toBools v2 == bools
      | otherwise = error "ooof"

-- test_InstThinIxEq :: Gen Bool
-- test_InstThinIxEq = do
--   Inductive.Nat.SomeSNat n <- arbitrary
--   Inductive.Nat.SomeSNat m <- arbitrary
--   th <- Inductive.arbitraryTh n m
--   ix <- Inductive.arbitraryIx n
--   pure $
--     Inductive.thin th ix ==
--     Inductive.Ix.toInductive
--       (Unsafe.thin
--         (Inductive.fromInductive th)
--         (Inductive.Ix.fromInductive ix))

-- test_InstThickIxEq :: Gen Bool
-- test_InstThickIxEq = do
--   Inductive.Nat.SomeSNat n <- arbitrary
--   Inductive.Nat.SomeSNat m <- arbitrary
--   th <- Inductive.arbitraryTh n m
--   ix <- Inductive.arbitraryIx m
--   pure $
--     Inductive.thick th ix ==
--     Inductive.Ix.toInductive
--       (Unsafe.thick
--         (Inductive.fromInductive th)
--         (Inductive.Ix.fromInductive ix))


-- test_InstThinThEq :: Gen Bool
-- test_InstThinThEq = do
--    Inductive.Nat.SomeSNat n <- arbitrary
--    Inductive.Nat.SomeSNat m <- arbitrary
--    Inductive.Nat.SomeSNat l <- arbitrary
--    th1 <- Inductive.arbitraryTh n m
--    th2 <- Inductive.arbitraryTh l n
--    pure $
--      Inductive.thin th1 th2 ==
--      Inductive.toInductive
--        (Unsafe.thin
--         (Inductive.fromInductive th1)
--         (Inductive.fromInductive th2))


-- test_InstThickThEq :: Gen Bool
-- test_InstThickThEq = do
--    Inductive.Nat.SomeSNat n <- arbitrary
--    Inductive.Nat.SomeSNat m <- arbitrary
--    Inductive.Nat.SomeSNat l <- arbitrary
--    th1 <- Inductive.arbitraryTh n m
--    th2 <- Inductive.arbitraryTh l m
--    pure $
--      Inductive.thick th1 th2 ==
--      Inductive.toInductive
--        (Unsafe.thick
--         (Inductive.fromInductive th1)
--         (Inductive.fromInductive th2))
