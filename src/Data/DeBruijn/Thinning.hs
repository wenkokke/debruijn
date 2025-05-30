{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Data.DeBruijn.Thinning (
  -- * Thinnings
  (:<=) (KeepAll, KeepOne, DropOne),
  dropAll,
  toBools,
  fromTh,
  fromThRaw,

  -- * Existential Wrapper
  SomeTh (..),
  fromBools,
  toSomeTh,
  toSomeThRaw,

  -- * The action of thinnings on 'Nat'-indexed types
  Thin (..),

  -- * Specialised target for conversion
  ThRep,
) where

#ifdef EXPORT_SAFE_API
import Data.DeBruijn.Thinning.Safe (
  SomeTh (..),
  Thin (..),
  dropAll,
  toSomeTh,
  toSomeThRaw,
  fromBools,
  toBools,
  fromTh,
  fromThRaw,
  (:<=) (DropOne, KeepAll, KeepOne),
  ThRep,
 )
#else
import Data.DeBruijn.Thinning.Fast (
  SomeTh (..),
  Thin (..),
  dropAll,
  toSomeTh,
  toSomeThRaw,
  fromBools,
  toBools,
  fromTh,
  fromThRaw,
  (:<=) (DropOne, KeepAll, KeepOne),
  ThRep,
 )
#endif
