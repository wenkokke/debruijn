{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Test.Data.DeBruijn.Thinning (
  tests,
) where

import Data.DeBruijn.Index.Inductive qualified as Inductive (Ix)
import Data.DeBruijn.Index.Inductive qualified as Unsafe.Ix (fromInductive, toInductive)
import Data.DeBruijn.Index.Inductive.Arbitrary qualified as Inductive (arbitraryIx)
import Data.DeBruijn.Thinning.Inductive qualified as Inductive
import Data.DeBruijn.Thinning.Inductive qualified as Unsafe (fromInductive, toInductive)
import Data.DeBruijn.Thinning.Inductive.Arbitrary ()
import Data.DeBruijn.Thinning.Inductive.Arbitrary qualified as Inductive
import Data.DeBruijn.Thinning.Unsafe qualified as Unsafe
import Data.Type.Nat.Singleton.Inductive qualified as Inductive (SNat (..), SomeSNat (..), plus)
import Data.Type.Nat.Singleton.Inductive qualified as Unsafe.SNat (fromInductive)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Property, counterexample, once, testProperty)
import Text.Printf (printf)

tests :: TestTree
tests =
  testGroup
    "Test.Data.DeBruijn.Thinning"
    [ testProperty "test_doneTh" test_doneTh
    , testProperty "test_keepTh" test_keepTh
    , testProperty "test_dropTh" test_dropTh
    , testProperty "test_keepAllEq" test_keepAllEq
    , testProperty "test_dropAllEq" test_dropAllEq
    , testProperty "test_toBoolsEq" test_toBoolsEq
    , testProperty "test_thinIxEq" test_thinIxEq
    , testProperty "test_thickIxEq" test_thickIxEq
    , testProperty "test_thinThEq" test_thinThEq
    ]

test_doneTh :: Property
test_doneTh =
  once $
    Inductive.Done == Unsafe.toInductive Unsafe.Done

test_keepTh :: Inductive.SomeTh -> Property
test_keepTh (Inductive.SomeTh _n _m nm) = do
  let expect = Inductive.Keep nm
  let actual = Unsafe.toInductive (Unsafe.Keep (Unsafe.fromInductive nm))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_dropTh :: Inductive.SomeTh -> Property
test_dropTh (Inductive.SomeTh _n _m nm) = do
  let expect = Inductive.Drop nm
  let actual = Unsafe.toInductive (Unsafe.Drop (Unsafe.fromInductive nm))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_keepAllEq :: Inductive.SomeSNat -> Property
test_keepAllEq (Inductive.SomeSNat n) = do
  let expect = Inductive.keepAll n
  let actual = Unsafe.toInductive (Unsafe.keepAll (Unsafe.SNat.fromInductive n))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_dropAllEq :: Inductive.SomeSNat -> Property
test_dropAllEq (Inductive.SomeSNat n) = do
  let expect = Inductive.dropAll n
  let actual = Unsafe.toInductive (Unsafe.dropAll (Unsafe.SNat.fromInductive n))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

test_toBoolsEq :: Inductive.SomeTh -> Property
test_toBoolsEq (Inductive.SomeTh _n _m nm) = do
  let expect = Inductive.toBools nm
  let actual = Unsafe.toBools (Unsafe.fromInductive nm)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

data SomeThinIxArgs = forall n m. SomeThinIxArgs (n Inductive.:<= m) (Inductive.Ix n)

deriving stock instance Show SomeThinIxArgs

instance Arbitrary SomeThinIxArgs where
  arbitrary :: Gen SomeThinIxArgs
  arbitrary = do
    Inductive.SomeSNat n <- arbitrary
    Inductive.SomeSNat m <- arbitrary
    SomeThinIxArgs <$> Inductive.arbitraryTh (Inductive.S n) m <*> Inductive.arbitraryIx (Inductive.S n)

test_thinIxEq :: SomeThinIxArgs -> Property
test_thinIxEq (SomeThinIxArgs nm i) = do
  let expect = Inductive.thin nm i
  let actual = Unsafe.Ix.toInductive (Unsafe.thin (Unsafe.fromInductive nm) (Unsafe.Ix.fromInductive i))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

data SomeThickIxArgs = forall n m. SomeThickIxArgs (n Inductive.:<= m) (Inductive.Ix m)

deriving stock instance Show SomeThickIxArgs

instance Arbitrary SomeThickIxArgs where
  arbitrary :: Gen SomeThickIxArgs
  arbitrary = do
    Inductive.SomeSNat n <- arbitrary
    Inductive.SomeSNat m <- arbitrary
    SomeThickIxArgs <$> Inductive.arbitraryTh (Inductive.S n) m <*> Inductive.arbitraryIx (Inductive.S (n `Inductive.plus` m))

test_thickIxEq :: SomeThickIxArgs -> Property
test_thickIxEq (SomeThickIxArgs nm i) = do
  let expect = Inductive.thick nm i
  let actual = Unsafe.Ix.toInductive <$> Unsafe.thick (Unsafe.fromInductive nm) (Unsafe.Ix.fromInductive i)
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

data SomeThinThArgs = forall l n m. SomeThinThArgs (n Inductive.:<= m) (l Inductive.:<= n)

deriving stock instance Show SomeThinThArgs

instance Arbitrary SomeThinThArgs where
  arbitrary :: Gen SomeThinThArgs
  arbitrary = do
    Inductive.SomeSNat l <- arbitrary
    Inductive.SomeSNat dn <- arbitrary
    let n = l `Inductive.plus` dn
    Inductive.SomeSNat dm <- arbitrary
    SomeThinThArgs <$> Inductive.arbitraryTh n dm <*> Inductive.arbitraryTh l dn

test_thinThEq :: SomeThinThArgs -> Property
test_thinThEq (SomeThinThArgs nm ln) = do
  let expect = Inductive.thin nm ln
  let actual = Unsafe.toInductive (Unsafe.thin (Unsafe.fromInductive nm) (Unsafe.fromInductive ln))
  counterexample (printf "%s == %s" (show expect) (show actual)) $
    expect == actual

-- TODO: test_thickThEq
-- This test is incredibly annoying to write, because its inputs are two
-- thinnings @n :< m@ and @l :< m@, which cannot be obtained with the current
-- operations on type-level natural numbers.
