{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Data.DeBruijn.Thinning.Full.Safe
  ( -- * Thinnings
    (:<=) (Done, Keep, Drop)
  , keepAll
  , dropAll
  , decFullTh
  , toBools
  , toBits

  -- * Existential Wrapper
  , SomeFullTh (..)
  , fromBools
  , fromBits

  -- * The action of thinnings on 'Nat'-indexed types
  , Thin (..)
  ) where

import Data.Kind (Type, Constraint)
import Data.Type.Nat (Nat (..))
import Control.DeepSeq (NFData (..))
import Data.Type.Nat.Singleton.Safe (KnownNat (..), SNat (..))
import Data.Bits (Bits (..))
import Numeric.Natural (Natural)
import Data.Word (Word64)
import Data.Type.Equality (type (:~:) (Refl), apply)
import Data.Bifunctor (Bifunctor(..))
import Data.Maybe (isJust)
import Data.DeBruijn.Index.Safe (Ix (..), isPos)

type (:<=) :: Nat -> Nat -> Type
data (:<=) n m where
  Done :: Z :<= Z
  Keep :: n :<= m -> S n :<= S m
  Drop :: n :<= m -> n :<= S m

deriving stock instance Eq (n :<= m)

deriving stock instance Show (n :<= m)

instance NFData (n :<= m) where
  rnf :: (n :<= m) -> ()
  rnf Done = ()
  rnf (Keep n'm') = rnf n'm'
  rnf (Drop nm') = rnf nm'

-- | Keep all entries.
keepAll :: KnownNat n => n :<= n
keepAll = go natSing
  where
    go :: SNat n -> n :<= n
    go Z = Done
    go (S n) = Keep (go n)

-- | Drop all entries.
dropAll :: KnownNat n => Z :<= n
dropAll = go natSing
  where
    go :: SNat n -> Z :<= n
    go Z = Done
    go (S n) = Drop (go n)

decFullTh :: n1 :<= m1 -> n2 :<= m2 -> Maybe (n1 :~: n1, m1 :~: m2)
decFullTh Done Done = Just (Refl, Refl)
decFullTh (Keep n1'm1') (Keep n2'm2') = bimap (apply Refl) (apply Refl) <$> decFullTh n1'm1' n2'm2'
decFullTh (Drop n1m1') (Drop n2m2') = second (apply Refl) <$> decFullTh n1m1' n2m2'
decFullTh _ _ = Nothing

-- | Convert a full thinning into a list of booleans.
toBools :: n :<= m -> [Bool]
toBools = \case
  Done -> []
  Keep n'm' -> False : toBools n'm'
  Drop nm' -> True : toBools nm'

-- | Convert a thinning into a bit sequence.
toBits :: (Bits bs) => n :<= m -> bs
toBits = \case
  Done -> zeroBits
  Keep n'm' -> (`shift` 1) . toBits $ n'm'
  Drop nm' -> (`setBit` 0) . (`shift` 1) . toBits $ nm'
{-# SPECIALIZE toBits :: n :<= m -> Natural #-}
{-# SPECIALIZE toBits :: n :<= m -> Word64 #-}

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

data SomeFullTh = forall n m. SomeFullTh !(n :<= m)

instance Eq SomeFullTh where
  (==) :: SomeFullTh -> SomeFullTh -> Bool
  SomeFullTh n1m1 == SomeFullTh n2m2 = isJust (decFullTh n1m1 n2m2)

deriving stock instance Show SomeFullTh

instance NFData SomeFullTh where
  rnf :: SomeFullTh -> ()
  rnf (SomeFullTh nm)= rnf nm

someDone :: SomeFullTh
someDone = SomeFullTh Done

someKeep :: SomeFullTh -> SomeFullTh
someKeep (SomeFullTh n'm') = SomeFullTh (Keep n'm')

someDrop :: SomeFullTh -> SomeFullTh
someDrop (SomeFullTh n'm') = SomeFullTh (Drop n'm')

fromBools :: [Bool] -> SomeFullTh
fromBools = go
 where
  go [] = someDone
  go (False : bools) = someKeep (go bools)
  go (True : bools) = someDrop (go bools)

fromBits :: (Integral n, Bits bs) => (n, bs) -> SomeFullTh
fromBits = uncurry go
  where
    go !n !bs
      | n <= 0 = someDone
      | testBit bs 0 = someDrop (go (n - 1) (shift bs (-1)))
      | otherwise = someKeep (go (n - 1) (shift bs (-1)))

--------------------------------------------------------------------------------
-- Thinning Class
--------------------------------------------------------------------------------

-- | The actions of thinnings on natural-indexed data types.
type Thin :: (Nat -> Type) -> Constraint
class Thin f where
  thin :: n :<= m -> f n -> f m
  thick :: n :<= m -> f m -> Maybe (f n)

instance Thin Ix where
  thin :: n :<= m -> Ix n -> Ix m
  thin !nm !i = isPos i $
    case nm of
      Keep n'm' ->
        case i of
          FZ -> FZ
          FS i' -> FS (thin n'm' i')
      Drop nm' -> FS (thin nm' i)

  thick :: n :<= m -> Ix m -> Maybe (Ix n)
  thick !nm !i = isPos i $
    case (nm, i) of
      (Keep _n'm', FZ) -> Just FZ
      (Keep n'm', FS i') -> FS <$> thick n'm' i'
      (Drop _nm', FZ) -> Nothing
      (Drop nm', FS i') -> thick nm' i'

instance Thin ((:<=) l) where
  thin :: n :<= m -> l :<= n -> l :<= m
  thin nm Done = nm
  thin (Keep n'm') (Keep l'n') = Keep (thin n'm' l'n')
  thin (Keep n'm') (Drop ln') = Drop (thin n'm' ln')
  thin (Drop nm') ln = Drop (thin nm' ln)

  thick :: n :<= m -> l :<= m -> Maybe (l :<= n)
  thick Done lm = Just lm
  thick (Keep n'm') (Keep l'n') = Keep <$> thick n'm' l'n'
  thick (Keep n'm') (Drop ln') = Drop <$> thick n'm' ln'
  thick (Drop _nm') (Keep _l'n') = Nothing
  thick (Drop nm') (Drop ln') = thick nm' ln'
