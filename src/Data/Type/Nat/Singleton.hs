{-# LANGUAGE CPP #-}

module Data.Type.Nat.Singleton (
  -- * Natural Number Singletons
  SNat (Z, S),
  fromSNat,
  fromSNatRaw,
  plus,
  decSNat,

  -- * Existential Wrapper
  SomeSNat (..),
  withSomeSNat,
  toSomeSNat,
  toSomeSNatRaw,
  fromSomeSNat,
  fromSomeSNatRaw,

  -- * Laws
  plusUnitL,
  plusUnitR,
  plusCommS,
  plusComm,
  plusAssoc,

  -- * Linking Type-Level and Value-Level
  KnownNat (..),
  withKnownNat,

  -- * Specialised target for conversion
  SNatRep,
) where

#ifdef EXPORT_SAFE_API
import Data.Type.Nat.Singleton.Safe (
  SNat (S, Z),
  SomeSNat (..),
  plus,
  decSNat,
  fromSNat,
  fromSNatRaw,
  fromSomeSNat,
  fromSomeSNatRaw,
  toSomeSNat,
  toSomeSNatRaw,
  withSomeSNat,
  SNatRep,
  KnownNat (..),
  withKnownNat,
  plusUnitL,
  plusUnitR,
  plusCommS,
  plusComm,
  plusAssoc,
 )
#else
import Data.Type.Nat.Singleton.Fast (
  SNat (S, Z),
  SomeSNat (..),
  plus,
  decSNat,
  fromSNat,
  fromSNatRaw,
  fromSomeSNat,
  fromSomeSNatRaw,
  toSomeSNat,
  toSomeSNatRaw,
  withSomeSNat,
  KnownNat (..),
  withKnownNat,
  SNatRep,
  plusUnitL,
  plusUnitR,
  plusCommS,
  plusComm,
  plusAssoc,
 )
#endif
