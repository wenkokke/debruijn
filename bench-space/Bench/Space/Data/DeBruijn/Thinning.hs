{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Bench.Space.Data.DeBruijn.Thinning where

import Bench.Samples.Data.DeBruijn.Thinning (samples1, samples2, samples3)
import Control.DeepSeq (force)
import Data.DeBruijn.Thinning (SomeTh (..), Thin (..), toSomeTh, (:<=))
import Data.Foldable (traverse_)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat.Singleton (decSNat, fromSNatRaw)
import Text.Printf (printf)
import Weigh (Weigh, func', wgroup)

benchmarks :: Weigh ()
benchmarks =
  wgroup "Data.DeBruijn.Thinning" $ do
    bench_thin

bench_thin :: Weigh ()
bench_thin =
  wgroup "thin" $ do
    wgroup "samples1" $ do
      traverse_ bench_thinWith samples1
    wgroup "samples2" $ do
      traverse_ bench_thinWith samples2
    wgroup "samples3" $ do
      traverse_ bench_thinWith samples3

data SomeThinThArgs = forall l n m. SomeThinThArgs (n :<= m) (l :<= n)

deriving stock instance Show SomeThinThArgs

bench_thinWith :: (Int, Integer, Int, Integer, Int) -> Weigh ()
bench_thinWith (lRep, lnRep, nRep, nmRep, mRep)
  | let !benchLabel = "[" <> show lRep <> "," <> showThRep nRep lnRep <> "," <> show nRep <> "," <> showThRep mRep nmRep <> "," <> show mRep <> "]" :: String
  , SomeTh n _m nm <- force (toSomeTh (nRep, nmRep))
  , SomeTh _l n' ln <- force (toSomeTh (lRep, lnRep)) =
      case decSNat n n' of
        Just Refl -> func' benchLabel (thin nm) ln
        Nothing -> error (printf msgFormat lRep lnRep nRep nmRep mRep (fromSNatRaw n) (fromSNatRaw n'))
 where
  showThRep n = printf ("0b%0" <> show n <> "b")
  msgFormat = "toSomeThinThArgs: could not construct SomeThinThArgs from " <> argFormat <> ": " <> errFormat
  argFormat = "(%d, 0b%0" <> show nRep <> "b, %d, 0b%0" <> show mRep <> "b, %d)"
  errFormat = "%d /= %d"
