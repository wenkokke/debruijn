{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Bench.Space.Data.DeBruijn.Index where

import Control.DeepSeq (force)
import Data.DeBruijn.Index (IxRep, SomeIx (..), thick, thin, toSomeIxRaw)
import Data.Foldable (traverse_)
import Data.List (nub)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat.Singleton (SNat (..), SNatRep, decSNat)
import Text.Printf (printf)
import Weigh (Weigh, func', wgroup)

benchmarks :: Weigh ()
benchmarks =
  wgroup "Data.DeBruijn.Thinning" $ do
    bench_thin

--------------------------------------------------------------------------------
-- Benchmark: thin
--------------------------------------------------------------------------------

bench_thin :: Weigh ()
bench_thin = wgroup "thin" (traverse_ bench_thinWith bench_thinArgs)

bench_thinWith :: (SNatRep, IxRep, IxRep) -> Weigh ()
bench_thinWith (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , SomeIx sn i <- force (toSomeIxRaw (nRaw + 1, iRaw))
  , SomeIx n j <- force (toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- decSNat sn (S n) =
      func' benchLabel (thin i) j
  | otherwise = error (printf "bench_thinWith(%d,%d,%d): could not construct benchmark" nRaw iRaw jRaw)

bench_thinArgs :: [(SNatRep, IxRep, IxRep)]
bench_thinArgs = nub (evenSpreadBy5 <> alongTheDiagonal)
 where
  evenSpreadBy5 =
    [ (101, i, j)
    | i <- [0, 5 .. 100]
    , j <- [0, 5 .. 100]
    ]
  alongTheDiagonal =
    [ (101, i, i)
    | i <- [0 .. 100]
    ]

--------------------------------------------------------------------------------
-- Benchmark: thick
--------------------------------------------------------------------------------

bench_thick :: Weigh ()
bench_thick = wgroup "thick" (traverse_ bench_thickWith bench_thickArgs)

bench_thickWith :: (SNatRep, IxRep, IxRep) -> Weigh ()
bench_thickWith (nRaw, iRaw, jRaw)
  | let !benchLabel = printf "[%d,%d]" iRaw jRaw :: String
  , SomeIx (S n) i <- force (toSomeIxRaw (nRaw, iRaw))
  , SomeIx (S n') j <- force (toSomeIxRaw (nRaw, jRaw))
  , Just Refl <- decSNat n n' =
      func' benchLabel (thick i) j
  | otherwise = error (printf "bench_thickWith(%d,%d,%d): could not construct benchmark" nRaw iRaw jRaw)

bench_thickArgs :: [(SNatRep, IxRep, IxRep)]
bench_thickArgs = nub (evenSpreadBy5 <> alongTheDiagonal)
 where
  evenSpreadBy5 =
    [ (101, i, j)
    | i <- [0, 5 .. 100]
    , j <- [0, 5 .. 100]
    ]
  alongTheDiagonal =
    [ (101, i, i)
    | i <- [0 .. 100]
    ]
