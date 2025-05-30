{-# LANGUAGE CPP #-}

module Data.DeBruijn.Index (
  -- * DeBruijn Indexes
  Ix (FZ, FS),
  eqIx,
  fromIx,
  fromIxRaw,
  isPos,
  thin,
  thick,
  inject,
  raise,

  -- * Existential Wrapper
  SomeIx (..),
  withSomeIx,
  toSomeIx,
  toSomeIxRaw,
  fromSomeIx,
  fromSomeIxRaw,

  -- * Specialised target for conversion
  IxRep,
) where

#ifdef EXPORT_SAFE_API
import Data.DeBruijn.Index.Safe (
  Ix (FS, FZ),
  SomeIx (..),
  eqIx,
  fromIx,
  fromIxRaw,
  fromSomeIx,
  fromSomeIxRaw,
  inject,
  isPos,
  raise,
  thick,
  thin,
  toSomeIx,
  toSomeIxRaw,
  withSomeIx,
  IxRep,
 )
#else
import Data.DeBruijn.Index.Fast (
  Ix (FS, FZ),
  SomeIx (..),
  eqIx,
  fromIx,
  fromIxRaw,
  fromSomeIx,
  fromSomeIxRaw,
  inject,
  isPos,
  raise,
  thick,
  thin,
  toSomeIx,
  toSomeIxRaw,
  withSomeIx,
  IxRep,
 )
#endif
