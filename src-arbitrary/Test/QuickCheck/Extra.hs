{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Test.QuickCheck.Extra (
  chooseSizedBoundedIntegral,
  chooseSizesBoundedPositiveIntegral,
) where

import Control.Exception (assert)
import Data.Bits (Bits (..))
import Test.QuickCheck.Gen (Gen, chooseInteger, sized)

chooseSizedBoundedIntegral :: (Integral a) => (a, a) -> Gen a
chooseSizedBoundedIntegral (mn, mx) =
  assert (mn >= 0 && mx >= mn) $
    let ilog2 1 = 0
        ilog2 n | n > 0 = 1 + ilog2 (n `div` 2)

        -- How many bits are needed to represent this type?
        -- (This number is an upper bound, not exact.)
        bits = ilog2 (toInteger mx - toInteger mn + 1)
    in  sized $ \k ->
          let
            -- Reach maximum size by k=80, or quicker for small integer types
            power = ((bits `max` 40) * k) `div` 80

            -- Bounds should be 2^power, but:
            --   * clamp the result to minBound/maxBound
            --   * clamp power to 'bits', in case k is a huge number
            lo = toInteger mn `max` (-1 `shiftL` (power `min` bits))
            hi = toInteger mx `min` (1 `shiftL` (power `min` bits))
          in
            fmap fromInteger (chooseInteger (lo, hi))
{-# INLINEABLE chooseSizedBoundedIntegral #-}
{-# ANN chooseSizedBoundedIntegral ("HLint: ignore Avoid partial function" :: String) #-}
{-# ANN chooseSizedBoundedIntegral ("HLint: ignore Parenthesize unary negation" :: String) #-}

chooseSizesBoundedPositiveIntegral :: (Integral a) => a -> Gen a
chooseSizesBoundedPositiveIntegral n = chooseSizedBoundedIntegral (1, n)
