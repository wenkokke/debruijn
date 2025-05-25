{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Type.Nat (
  type Nat (..),
  type (+),
  type Pred,
  type Pos,
  type (<=),
) where

import Data.Kind (Constraint)

-- | Type-level natural numbers.
type data Nat = Z | S Nat

-- | Addition of type-level naturals.
type (+) :: Nat -> Nat -> Nat
type family (+) n m where
  Z + m = m
  S n + m = S (n + m)

-- | Predecessor of type-level naturals.
type Pred :: Nat -> Nat
type family Pred n where
  Pred (S n) = n

-- | @'Pos' n@ holds if @n@ is non-zero.
type Pos :: Nat -> Constraint
type Pos (n :: Nat) = n ~ S (Pred n)

-- | Less-than-or-equal for type-level naturals.
type (<=) :: Nat -> Nat -> Bool
type family (<=) n m where
  Z <= m = 'True
  S n <= Z = 'False
  S n <= S m = n <= m
