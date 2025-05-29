{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Bench.Time.Data.DeBruijn.Index qualified (benchmarks)
import Bench.Time.Data.DeBruijn.Thinning qualified (benchmarks)
import Criterion.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.Time.Data.DeBruijn.Index.benchmarks
    , Bench.Time.Data.DeBruijn.Thinning.benchmarks
    ]
