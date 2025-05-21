{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeBruijn.Thinning.Inductive.Arbitrary (
  arbitraryTh,
) where

import Data.DeBruijn.Thinning.Inductive (SomeTh (..), dropAll, keepAll, type (:<=) (Drop, Keep))
import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat (type (+))
import Data.Type.Nat.Singleton.Inductive (SNat (..), SomeSNat (..), plus, plusCommS, plusUnitR)
import Data.Type.Nat.Singleton.Inductive.Arbitrary ()
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, oneof)

instance Arbitrary SomeTh where
  arbitrary :: Gen SomeTh
  arbitrary = do
    SomeSNat lower <- arbitrary
    SomeSNat delta <- arbitrary
    value <- arbitraryTh lower delta
    let upper = lower `plus` delta
    pure SomeTh{..}

arbitraryTh :: SNat n -> SNat m -> Gen (n :<= (n + m))
arbitraryTh n Z =
  case plusUnitR n of
    Refl -> pure (keepAll n)
arbitraryTh Z m =
  pure (dropAll m)
arbitraryTh n@(S n') m@(S m') =
  oneof [keep1, drop1]
 where
  keep1 =
    Keep <$> arbitraryTh n' m
  drop1 =
    case plusCommS n' (erase m') of
      Refl -> Drop <$> arbitraryTh n m'

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | @`erase` x@ erases the content of @x@ to a @`Proxy`@.
erase :: f a -> Proxy a
erase _ = Proxy
{-# INLINE erase #-}
