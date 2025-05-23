{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Bench.Time.Data.DeBruijn.Index (
  benchmarks,
) where

import Control.DeepSeq (force)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.DeBruijn.Index.Inductive qualified as Inductive
import Data.DeBruijn.Index.Unsafe qualified as Unsafe
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat.Singleton.Inductive qualified as Inductive
import Data.Type.Nat.Singleton.Unsafe qualified as Unsafe
import Text.Printf (printf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Bench.Data.DeBruijn.Index"
    [ bench_thin
    , bench_thick
    ]

--------------------------------------------------------------------------------
-- Benchmark: thin
--------------------------------------------------------------------------------

bench_thin :: Benchmark
bench_thin =
  bgroup
    "thin"
    [ bgroup "Unsafe" (bench_thin_Unsafe <$> bench_thin_Args)
    , bgroup "Inductive" (bench_thin_Inductive <$> bench_thin_Args)
    ]

bench_thin_Unsafe :: (Int, Int, Int) -> Benchmark
bench_thin_Unsafe (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , Unsafe.SomeIx sn i <- force (Unsafe.toSomeIxRaw (nRaw + 1, iRaw))
  , Unsafe.SomeIx n j <- force (Unsafe.toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- Unsafe.decSNat sn (Unsafe.S n) =
      bench benchLabel $ nf (Unsafe.thin i) j
  | otherwise = error "bench_thin_Unsafe: could not construct benchmark"

bench_thin_Inductive :: (Int, Int, Int) -> Benchmark
bench_thin_Inductive (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , Inductive.SomeIx sn i <- force (Inductive.toSomeIxRaw (nRaw + 1, iRaw))
  , Inductive.SomeIx n j <- force (Inductive.toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- Inductive.decSNat sn (Inductive.S n) =
      bench benchLabel $ nf (Inductive.thin i) j
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

bench_thick :: Benchmark
bench_thick =
  bgroup
    "thick"
    [ bgroup "Unsafe" (bench_thick_Unsafe <$> bench_thick_Args)
    , bgroup "Inductive" (bench_thick_Inductive <$> bench_thick_Args)
    ]

bench_thick_Unsafe :: (Int, Int, Int) -> Benchmark
bench_thick_Unsafe (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , Unsafe.SomeIx (Unsafe.S n) i <- force (Unsafe.toSomeIxRaw (nRaw, iRaw))
  , Unsafe.SomeIx (Unsafe.S n') j <- force (Unsafe.toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- Unsafe.decSNat n n' =
      bench benchLabel $ nf (Unsafe.thick i) j
  | otherwise = error "bench_thick_Unsafe: could not construct benchmark"

bench_thick_Inductive :: (Int, Int, Int) -> Benchmark
bench_thick_Inductive (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , Inductive.SomeIx (Inductive.S n) i <- force (Inductive.toSomeIxRaw (nRaw, iRaw))
  , Inductive.SomeIx (Inductive.S n') j <- force (Inductive.toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- Inductive.decSNat n n' =
      bench benchLabel $ nf (Inductive.thick i) j
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
