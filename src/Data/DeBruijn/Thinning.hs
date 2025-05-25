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
 )
#endif
