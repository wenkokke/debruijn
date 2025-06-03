{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeBruijn.Thinning.Safe.Arbitrary (
  arbitraryTh,
  SomeThinIxArgs (..),
  SomeThickIxArgs (..),
  SomeThinThArgs (..),
) where

import Data.DeBruijn.Index.Safe (Ix)
import Data.DeBruijn.Index.Safe.Arbitrary (arbitraryIx)
import Data.DeBruijn.Thinning.Arbitrary (SomeThBoundRep (..), SomeThRep (..))
import Data.DeBruijn.Thinning.Safe (SomeTh (..), dropAll, toSomeThRaw, type (:<=) (DropOne, KeepAll, KeepOne))
import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat (type (+))
import Data.Type.Nat.Singleton.Safe (SNat (..), SomeSNat (..), plusCommS, plusUnitR, toSomeSNatRaw)
import Data.Type.Nat.Singleton.Safe qualified as Safe
import Data.Type.Nat.Singleton.Safe.Arbitrary ()
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, oneof)

instance Arbitrary SomeTh where
  arbitrary :: Gen SomeTh
  arbitrary = do
    SomeThRep n nm <- arbitrary
    pure $ toSomeThRaw (n, nm)

arbitraryTh :: SNat n -> SNat d -> Gen (n :<= (n + d))
arbitraryTh n Z = case plusUnitR n of Refl -> pure KeepAll
arbitraryTh Z m = pure (dropAll m)
arbitraryTh n@(S n') m@(S m') = oneof [keepOne, dropOne]
 where
  keepOne = KeepOne <$> arbitraryTh n' m
  dropOne = case plusCommS n' (erase m') of Refl -> DropOne <$> arbitraryTh n m'

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeThBound

data SomeThBound = forall n. SomeThBound !(SNat n)

deriving stock instance Show SomeThBound

instance Arbitrary SomeThBound where
  arbitrary :: Gen SomeThBound
  arbitrary = do
    SomeThBoundRep nRep <- arbitrary
    pure $
      case toSomeSNatRaw nRep of
        SomeSNat n -> SomeThBound n

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeThinIxArgs

data SomeThinIxArgs = forall n m. SomeThinIxArgs (SNat n) (SNat m) (n :<= m) (Ix n)

deriving stock instance Show SomeThinIxArgs

instance Arbitrary SomeThinIxArgs where
  arbitrary :: Gen SomeThinIxArgs
  arbitrary = do
    SomeThBound n' <- arbitrary
    SomeThBound d <- arbitrary
    let n = Safe.S n'
    let m = n `Safe.plus` d
    SomeThinIxArgs n m <$> arbitraryTh n d <*> arbitraryIx n

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeThickIxArgs

data SomeThickIxArgs = forall n m. SomeThickIxArgs (SNat n) (SNat m) (n :<= m) (Ix m)

deriving stock instance Show SomeThickIxArgs

instance Arbitrary SomeThickIxArgs where
  arbitrary :: Gen SomeThickIxArgs
  arbitrary = do
    SomeThBound n' <- arbitrary
    SomeThBound d <- arbitrary
    let n = Safe.S n'
    let m = n `Safe.plus` d
    SomeThickIxArgs n m <$> arbitraryTh n d <*> arbitraryIx m

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeThinThArgs

data SomeThinThArgs = forall l n m. SomeThinThArgs (SNat l) (SNat n) (SNat m) (n :<= m) (l :<= n)

deriving stock instance Show SomeThinThArgs

instance Arbitrary SomeThinThArgs where
  arbitrary :: Gen SomeThinThArgs
  arbitrary = do
    Safe.SomeSNat l <- arbitrary
    Safe.SomeSNat dn <- arbitrary
    let n = l `Safe.plus` dn
    Safe.SomeSNat dm <- arbitrary
    let m = n `Safe.plus` dm
    SomeThinThArgs l n m <$> arbitraryTh n dm <*> arbitraryTh l dn

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | @`erase` x@ erases the content of @x@ to a @`Proxy`@.
erase :: f a -> Proxy a
erase _ = Proxy
{-# INLINE erase #-}
