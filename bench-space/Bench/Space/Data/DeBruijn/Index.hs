{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Bench.Space.Data.DeBruijn.Index where

import Control.DeepSeq (force)
import Data.DeBruijn.Index.Fast qualified as Fast
import Data.DeBruijn.Index.Safe qualified as Safe
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat.Singleton.Fast qualified as Fast
import Data.Type.Nat.Singleton.Safe qualified as Safe
import Text.Printf (printf)
import Weigh (Weigh, func', wgroup)

benchmarks :: Weigh ()
benchmarks =
  wgroup "Bench.Data.DeBruijn.Index" $ do
    bench_thin
    bench_thick

--------------------------------------------------------------------------------
-- Benchmark: thin
--------------------------------------------------------------------------------

bench_thin :: Weigh ()
bench_thin =
  wgroup "thin" $ do
    wgroup "Fast" $ do
      traverse_ bench_thin_Unsafe bench_thin_Args
    wgroup "Safe" $ do
      traverse_ bench_thin_Inductive bench_thin_Args

bench_thin_Unsafe :: (Int, Int, Int) -> Weigh ()
bench_thin_Unsafe (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , Fast.SomeIx sn i <- force (Fast.toSomeIxRaw (nRaw + 1, iRaw))
  , Fast.SomeIx n j <- force (Fast.toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- Fast.decSNat sn (Fast.S n) =
      func' benchLabel (Fast.thin i) j
  | otherwise = error "bench_thin_Unsafe: could not construct benchmark"

bench_thin_Inductive :: (Int, Int, Int) -> Weigh ()
bench_thin_Inductive (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , Safe.SomeIx sn i <- force (Safe.toSomeIxRaw (nRaw + 1, iRaw))
  , Safe.SomeIx n j <- force (Safe.toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- Safe.decSNat sn (Safe.S n) =
      func' benchLabel (Safe.thin i) j
  | otherwise = error "bench_thin_Inductive: could not construct benchmark"

bench_thin_Args :: [(Int, Int, Int)]
bench_thin_Args = nub (varyingParameter0 <> varyingParameter1)
 where
  varyingParameter0 =
    [ (101, i, j)
    | i <- [0, 10 .. 100]
    , j <- [0, i - 1, i, i + 1, 100]
    , 0 <= j && j < 101
    ]
  varyingParameter1 =
    varyingParameter0
      <&> (\(n, j, i) -> (n, i, j))

--------------------------------------------------------------------------------
-- Benchmark: thick
--------------------------------------------------------------------------------

bench_thick :: Weigh ()
bench_thick =
  wgroup "thick" $ do
    wgroup "Fast" $ do
      traverse_ bench_thick_Unsafe bench_thick_Args
    wgroup "Safe" $ do
      traverse_ bench_thick_Inductive bench_thick_Args

bench_thick_Unsafe :: (Int, Int, Int) -> Weigh ()
bench_thick_Unsafe (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , Fast.SomeIx (Fast.S n) i <- force (Fast.toSomeIxRaw (nRaw, iRaw))
  , Fast.SomeIx (Fast.S n') j <- force (Fast.toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- Fast.decSNat n n' =
      func' benchLabel (Fast.thick i) j
  | otherwise = error "bench_thick_Unsafe: could not construct benchmark"

bench_thick_Inductive :: (Int, Int, Int) -> Weigh ()
bench_thick_Inductive (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , Safe.SomeIx (Safe.S n) i <- force (Safe.toSomeIxRaw (nRaw, iRaw))
  , Safe.SomeIx (Safe.S n') j <- force (Safe.toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- Safe.decSNat n n' =
      func' benchLabel (Safe.thick i) j
  | otherwise = error "bench_thick_Inductive: could not construct benchmark"

bench_thick_Args :: [(Int, Int, Int)]
bench_thick_Args = nub (varyingParameter0 <> varyingParameter1)
 where
  varyingParameter0 =
    [ (101, i, j)
    | i <- [0, 10 .. 100]
    , j <- [0, i - 1, i, i + 1, 100]
    , 0 <= j && j < 101
    ]
  varyingParameter1 =
    varyingParameter0
      <&> (\(n, j, i) -> (n, i, j))
