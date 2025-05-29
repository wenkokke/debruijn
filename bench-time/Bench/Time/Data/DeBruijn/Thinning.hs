{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Bench.Time.Data.DeBruijn.Thinning where

import Bench.Time.Data.DeBruijn.Thinning.Samples (samples1)
import Control.DeepSeq (force)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.DeBruijn.Thinning (SomeTh (..), Thin (..), toSomeThRaw, (:<=))
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat.Singleton (decSNat, fromSNatRaw)
import Text.Printf (printf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Bench.Data.DeBruijn.Thinning"
    [ bench_thin
    ]

--------------------------------------------------------------------------------
-- Benchmark: thinTh
--------------------------------------------------------------------------------

bench_thin :: Benchmark
bench_thin = bgroup "thin" (bench_thinWith <$> samples1)

data SomeThinThArgs = forall l n m. SomeThinThArgs (n :<= m) (l :<= n)

deriving stock instance Show SomeThinThArgs

bench_thinWith :: (Int, Integer, Int, Integer, Int) -> Benchmark
bench_thinWith (lRep, lnRep, nRep, nmRep, mRep)
  | let !benchLabel = "[" <> show lRep <> "," <> showThRep nRep lnRep <> "," <> show nRep <> "," <> showThRep mRep nmRep <> "," <> show mRep <> "]" :: String
  , SomeTh n _m nm <- force (toSomeThRaw nRep nmRep)
  , SomeTh _l n' ln <- force (toSomeThRaw lRep lnRep) =
      case decSNat n n' of
        Just Refl -> bench benchLabel $ nf (thin nm) ln
        Nothing -> error (printf msgFormat lRep lnRep nRep nmRep mRep (fromSNatRaw n) (fromSNatRaw n'))
 where
  showThRep n = printf ("0b%0" <> show n <> "b")
  msgFormat = "toSomeThinThArgs: could not construct SomeThinThArgs from " <> argFormat <> ": " <> errFormat
  argFormat = "(%d, 0b%0" <> show nRep <> "b, %d, 0b%0" <> show mRep <> "b, %d)"
  errFormat = "%d /= %d"
