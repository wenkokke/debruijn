{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Bench.Space.Data.DeBruijn.Index where

import Control.DeepSeq (force)
import Data.DeBruijn.Index (SomeIx (..), thick, thin, toSomeIxRaw)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat.Singleton (SNat (..), decSNat)
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
bench_thin = wgroup "thin" (traverse_ bench_thinWith bench_thinArgs)

bench_thinWith :: (Int, Int, Int) -> Weigh ()
bench_thinWith (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , SomeIx sn i <- force (toSomeIxRaw (nRaw + 1, iRaw))
  , SomeIx n j <- force (toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- decSNat sn (S n) =
      func' benchLabel (thin i) j
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

bench_thick :: Weigh ()
bench_thick = wgroup "thick" (traverse_ bench_thickWith bench_thickArgs)

bench_thickWith :: (Int, Int, Int) -> Weigh ()
bench_thickWith (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , SomeIx (S n) i <- force (toSomeIxRaw (nRaw, iRaw))
  , SomeIx (S n') j <- force (toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- decSNat n n' =
      func' benchLabel (thick i) j
  | otherwise = error (printf "bench_thickWith(%d,%d,%d): could not construct benchmark" nRaw iRaw jRaw)

bench_thickArgs :: [(Int, Int, Int)]
bench_thickArgs = nub (varyingParameter0 <> varyingParameter1 <> alongTheDiagonal)
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
  alongTheDiagonal =
    [ (101, i, i)
    | i <- [0..100]
    ]
