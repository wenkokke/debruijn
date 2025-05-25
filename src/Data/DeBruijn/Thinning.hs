{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Data.DeBruijn.Thinning (
  -- * Thinnings
  (:<=) (KeepAll, KeepOne, DropOne),
  dropAll,
  toBools,

  -- * Existential Wrapper
  SomeTh (..),
  fromBools,
  fromBits,
  fromBitsRaw,

  -- * The action of thinnings on 'Nat'-indexed types
  Thin (..),
) where

#ifdef EXPORT_SAFE_API
import Data.DeBruijn.Thinning.Safe (
  SomeTh (..),
  Thin (..),
  dropAll,
  fromBits,
  fromBitsRaw,
  fromBools,
  toBools,
  (:<=) (DropOne, KeepAll, KeepOne),
 )
#else
import Data.DeBruijn.Thinning.Fast (
  SomeTh (..),
  Thin (..),
  dropAll,
  fromBits,
  fromBitsRaw,
  fromBools,
  toBools,
  (:<=) (DropOne, KeepAll, KeepOne),
 )
#endif
