{-# LANGUAGE ExplicitNamespaces #-}

module Data.DeBruijn.Thinning (
  (:<=) (Done, Keep, Drop),
  keepAll,
  dropAll,
  toBools,
  Thin (..),
) where

import Data.DeBruijn.Thinning.Unsafe (
  Thin (..),
  dropAll,
  keepAll,
  toBools,
  type (:<=) (Done, Drop, Keep),
 )
