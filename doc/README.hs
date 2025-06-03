{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module README where

import Data.Kind (Type)

--------------------------------------------------------------------------------
-- Safe Natural Numbers
--------------------------------------------------------------------------------
{-+}
data Nat
  = Z
  | S Nat

plus :: Nat -> Nat -> Nat
Z   `plus` m = m
S n `plus` m = S (n `plus` m)
{+-}
--------------------------------------------------------------------------------
-- Efficient Natural Number Representation
--------------------------------------------------------------------------------
{--}
type NatRep = Word

mkZRep :: NatRep -- Z
mkZRep = 0

mkSRep :: NatRep -> NatRep -- S
mkSRep = (+ 1)

unSRep :: NatRep -> NatRep
unSRep = subtract 1

elNatRep :: a -> (NatRep -> a) -> NatRep -> a
elNatRep ifZ ifS r =
  if r == mkZRep then ifZ else ifS (unSRep r)

{--}
--------------------------------------------------------------------------------
-- Efficient Natural Numbers
--------------------------------------------------------------------------------
{--}
newtype Nat = UnsafeNat {natRep :: NatRep}

mkZ :: Nat
mkZ = UnsafeNat mkZRep

mkS :: Nat -> Nat
mkS = UnsafeNat . mkSRep . (.natRep)

elNat :: a -> (Nat -> a) -> Nat -> a
elNat ifZ ifS =
  elNatRep ifZ (ifS . UnsafeNat) . (.natRep)

type NatF :: Type -> Type
data NatF nat where
  ZF :: NatF nat
  SF :: !nat -> NatF nat

projectNat :: Nat -> NatF Nat
projectNat =
  elNat ZF SF

embedNat :: NatF Nat -> Nat
embedNat = \case
  ZF -> mkZ
  SF n -> mkS n

pattern Z :: () => () => Nat
pattern Z <- (projectNat -> ZF) where Z = embedNat ZF

pattern S :: () => () => Nat -> Nat
pattern S n <- (projectNat -> SF n) where S n = embedNat (SF n)

{-# COMPLETE Z, S #-}

{--}
