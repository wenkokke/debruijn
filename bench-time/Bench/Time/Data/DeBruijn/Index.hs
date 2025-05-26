{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Bench.Time.Data.DeBruijn.Index (
  benchmarks,
  bench_thickArgs,
) where

import Control.DeepSeq (force)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.DeBruijn.Index (SomeIx (..), thick, thin, toSomeIxRaw)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat.Singleton (SNat (..), decSNat)
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
bench_thin = bgroup "thin" (bench_thinWith <$> bench_thinArgs)

bench_thinWith :: (Int, Int, Int) -> Benchmark
bench_thinWith (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , SomeIx sn i <- force (toSomeIxRaw (nRaw + 1, iRaw))
  , SomeIx n j <- force (toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- decSNat sn (S n) =
      bench benchLabel $ nf (thin i) j
  | otherwise = error (printf "bench_thinWith(%d,%d,%d): could not construct benchmark" nRaw iRaw jRaw)

bench_thinArgs :: [(Int, Int, Int)]
bench_thinArgs = nub (varyingParameter0 <> varyingParameter1)
 where
  varyingParameter0 =
    [ (101, i, j)
    | n <- [0, 5 .. 100]
    , i <- [0, n - 1, n, n + 1, 100]
    , 0 <= i && i < 101
    , j <- [0, n - 1, n, n + 1, 100]
    , 0 <= j && j < 101
    ]
  varyingParameter1 =
    varyingParameter0
      <&> (\(n, j, i) -> (n, i, j))

--------------------------------------------------------------------------------
-- Benchmark: thick
--------------------------------------------------------------------------------

bench_thick :: Benchmark
bench_thick = bgroup "thick" (bench_thickWith <$> bench_thickArgs)

bench_thickWith :: (Int, Int, Int) -> Benchmark
bench_thickWith (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , SomeIx (S n) i <- force (toSomeIxRaw (nRaw, iRaw))
  , SomeIx (S n') j <- force (toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- decSNat n n' =
      bench benchLabel $ nf (thick i) j
  | otherwise = error (printf "bench_thickWith(%d,%d,%d): could not construct benchmark" nRaw iRaw jRaw)

bench_thickArgs :: [(Int, Int, Int)]
bench_thickArgs = nub (varyingParameter0 <> varyingParameter1)
 where
  varyingParameter0 =
    [ (101, i, j)
    | n <- [0, 5 .. 100]
    , i <- [0, n - 1, n, n + 1, 100]
    , 0 <= i && i < 101
    , j <- [0, n - 1, n, n + 1, 100]
    , 0 <= j && j < 101
    ]
  varyingParameter1 =
    varyingParameter0
      <&> (\(n, j, i) -> (n, i, j))
