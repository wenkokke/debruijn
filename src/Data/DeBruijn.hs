{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.DeBruijn
  ( DB (Z, S),
    SomeDB (SomeDB, dbBound, dbIndex),
    SomeSNat (SomeSNat),
    SomePositiveSNat (SomePositiveSNat),
    fromDB,
    raise,
    inject,
    tabulate,
  )
where

import GHC.Num.Natural (Natural, naturalIsZero, naturalSubUnsafe, naturalZero)
import GHC.TypeNats (SNat, fromSNat, withSomeSNat, type (+))
import Test.QuickCheck.Modifiers (Positive (..), NonNegative (..))
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, elements)

-- | The type of DeBruijn indices.
newtype DB (n :: Natural) = UnsafeDB Natural
  deriving (Eq, Ord)

-- | Zero.
pattern Z :: forall n. DB (1 + n)
pattern Z <- (safePred -> Nothing)
  where
    Z = UnsafeDB naturalZero

-- | Successor.
pattern S :: forall n. DB n -> DB (1 + n)
pattern S db <- (safePred -> Just db)
  where
    S (UnsafeDB index) = UnsafeDB (1 + index)

-- | Take the precessor of the DeBruijn index.
safePred :: DB (1 + n) -> Maybe (DB n)
safePred (UnsafeDB index)
  | naturalIsZero index = Nothing
  | otherwise = Just (UnsafeDB (index `naturalSubUnsafe` 1))

{-# COMPLETE Z, S #-}

-- | Convert a DeBruijn index to a Natural.
fromDB :: DB n -> Natural
fromDB (UnsafeDB index) = index

instance Show (DB n) where
  show :: DB n -> String
  show (UnsafeDB index) = '#' : show index

-- | An existential wrapper for DeBruijn indices.
data SomeDB = forall n. SomeDB { dbBound :: SNat n, dbIndex :: DB n }

instance Show SomeDB where
  show :: SomeDB -> String
  show (SomeDB _bound index) = show index

-- | Raise the value of a DeBruijn index by some known natural `m`.
raise :: SNat m -> DB n -> DB (m + n)
raise m (UnsafeDB index) = UnsafeDB (fromSNat m + index)

-- | Raise the range of a DeBruijn index by some known natural `m`.
inject :: SNat m -> DB n -> DB (m + n)
inject _m (UnsafeDB index) = UnsafeDB index

-- | List all DeBruijn indices between `0` and some known natural `n`.
tabulate :: SNat n -> [DB n]
tabulate n = map UnsafeDB [0 .. fromSNat n]

instance Arbitrary SomeDB where
  arbitrary :: Gen SomeDB
  arbitrary = do
    SomePositiveSNat bound <- arbitrary
    elements [SomeDB bound index | index <- tabulate bound]

-- | This type represents unknown type-level natural number.
data SomeSNat = forall n. SomeSNat (SNat n)

deriving instance Show SomeSNat

instance Arbitrary SomeSNat where
  arbitrary :: Gen SomeSNat
  arbitrary = do
    n <- fromInteger . getNonNegative <$> arbitrary
    return $ withSomeSNat n SomeSNat

-- | This type represents unknown type-level positive number.
data SomePositiveSNat = forall n. SomePositiveSNat (SNat n)

deriving instance Show SomePositiveSNat

instance Arbitrary SomePositiveSNat where
  arbitrary :: Gen SomePositiveSNat
  arbitrary = do
    n <- fromInteger . getPositive <$> arbitrary
    return $ withSomeSNat n SomePositiveSNat
