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
 )
#endif
