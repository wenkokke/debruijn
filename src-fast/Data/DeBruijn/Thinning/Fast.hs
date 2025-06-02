{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.DeBruijn.Thinning.Fast (
  -- * Thinnings
  (:<=) (KeepAll, KeepOne, DropOne),
  dropAll,
  toBools,
  fromTh,
  fromThRaw,

  -- * Existential Wrapper
  SomeTh (..),
  fromBools,
  toSomeTh,
  toSomeThRaw,
  fromSomeTh,
  fromSomeThRaw,

  -- * The action of thinnings on 'Nat'-indexed types
  Thin (..),

  -- * Fast
  ThRep,
  bitsToThRep,
  thRepToBits,
  (:<=) (UnsafeTh, thRep),
) where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.DeBruijn.Index.Fast (Ix (..), isPos)
import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat (Nat (..), Pos, Pred)
import Data.Type.Nat.Singleton.Fast (SNat (..), SNatRep, SomeSNat (..), decSNat, plus, toSomeSNat, toSomeSNatRaw)
import Unsafe.Coerce (unsafeCoerce)

#if defined(TH_AS_BITVEC)
import Data.Bit (Bit)
import Data.Vector.Unboxed (Vector)
#elif defined(TH_AS_INTEGER)
-- No import needed for Integer
#elif defined(TH_AS_NATURAL)
import Numeric.Natural (Natural)
#elif defined(TH_AS_WORD64)
import Data.Word (Word64)
#endif

--------------------------------------------------------------------------------
-- Thinning Representation
--------------------------------------------------------------------------------

#if defined(TH_AS_BITVEC)
type ThRep = Vector Bit
#elif defined(TH_AS_INTEGER)
type ThRep = Integer
#elif defined(TH_AS_NATURAL)
type ThRep = Natural
#elif defined(TH_AS_WORD64)
type ThRep = Word64
#else
#error "cpp: define one of [TH_AS_BITVEC, TH_AS_INTEGER, TH_AS_NATURAL, TH_AS_WORD64]"
#endif

mkKeepAllRep :: ThRep
mkKeepAllRep = zeroBits
{-# INLINE mkKeepAllRep #-}

mkKeepOneRep :: ThRep -> ThRep
mkKeepOneRep = (`shift` 1)
{-# INLINE mkKeepOneRep #-}

mkDropOneRep :: ThRep -> ThRep
mkDropOneRep = (`setBit` 0) . (`shift` 1)
{-# INLINE mkDropOneRep #-}

elThRep :: a -> (ThRep -> a) -> (ThRep -> a) -> ThRep -> a
elThRep ifKeepAll ifKeepOne ifDropOne th
  | th == zeroBits = ifKeepAll
  | testBit th 0 = ifDropOne (shift th (-1))
  | otherwise = ifKeepOne (shift th (-1))
{-# INLINE elThRep #-}

--------------------------------------------------------------------------------
-- Thinnings
--------------------------------------------------------------------------------

type (:<=) :: Nat -> Nat -> Type
newtype (:<=) n m = UnsafeTh {thRep :: ThRep}

type role (:<=) nominal nominal

mkKeepAll :: n :<= n
mkKeepAll = UnsafeTh mkKeepAllRep
{-# INLINE mkKeepAll #-}

mkKeepOne :: n :<= m -> S n :<= S m
mkKeepOne = UnsafeTh . mkKeepOneRep . (.thRep)
{-# INLINE mkKeepOne #-}

mkDropOne :: n :<= m -> n :<= S m
mkDropOne = UnsafeTh . mkDropOneRep . (.thRep)
{-# INLINE mkDropOne #-}

elTh :: a -> (Pred n :<= Pred m -> a) -> (n :<= Pred m -> a) -> n :<= m -> a
elTh ifKeepAll ifKeepOne ifDropOne =
  elThRep ifKeepAll (ifKeepOne . UnsafeTh) (ifDropOne . UnsafeTh) . (.thRep)
{-# INLINE elTh #-}

data ThF (th :: Nat -> Nat -> Type) (n :: Nat) (m :: Nat) where
  KeepAllF :: ThF th n n
  KeepOneF :: !(th n m) -> ThF th (S n) (S m)
  DropOneF :: !(th n m) -> ThF th n (S m)

projectTh :: n :<= m -> ThF (:<=) n m
projectTh =
  elTh (unsafeCoerce KeepAllF) (unsafeCoerce . KeepOneF) (unsafeCoerce . DropOneF)
{-# INLINE projectTh #-}

embedTh :: ThF (:<=) n m -> n :<= m
embedTh = \case
  KeepAllF -> mkKeepAll
  KeepOneF n'm' -> mkKeepOne n'm'
  DropOneF nm' -> mkDropOne nm'
{-# INLINE embedTh #-}

pattern KeepAll :: () => (n ~ m) => n :<= m
pattern KeepAll <- (projectTh -> KeepAllF) where KeepAll = embedTh KeepAllF
{-# INLINE KeepAll #-}

pattern KeepOne :: () => (Pos n, Pos m) => Pred n :<= Pred m -> n :<= m
pattern KeepOne nm <- (projectTh -> KeepOneF nm) where KeepOne nm = embedTh (KeepOneF nm)
{-# INLINE KeepOne #-}

pattern DropOne :: () => (Pos m) => n :<= Pred m -> n :<= m
pattern DropOne nm <- (projectTh -> DropOneF nm) where DropOne nm = embedTh (DropOneF nm)
{-# INLINE DropOne #-}

{-# COMPLETE KeepAll, KeepOne, DropOne #-}

deriving newtype instance Eq (n :<= m)

instance Show (n :<= m) where
  showsPrec :: Int -> n :<= m -> ShowS
  showsPrec p =
    showParen (p > 10) . \case
      KeepAll -> showString "KeepAll"
      KeepOne n'm' -> showString "KeepOne " . showsPrec 11 n'm'
      DropOne nm' -> showString "DropOne " . showsPrec 11 nm'

deriving newtype instance NFData (n :<= m)

-- | Drop all entries.
dropAll :: SNat m -> Z :<= m
dropAll Z = KeepAll
dropAll (S m') = DropOne (dropAll m')

-- | Convert a thinning into a list of booleans.
toBools :: n :<= m -> [Bool]
toBools = \case
  KeepAll -> []
  KeepOne n'm' -> False : toBools n'm'
  DropOne nm' -> True : toBools nm'

-- | Convert a thinning into a bit sequence.
fromTh :: (Bits bs) => n :<= m -> bs
fromTh = \case
  KeepAll -> zeroBits
  KeepOne n'm' -> (`shift` 1) . fromTh $ n'm'
  DropOne nm' -> (`setBit` 0) . (`shift` 1) . fromTh $ nm'
{-# SPECIALIZE fromTh :: n :<= m -> ThRep #-}

fromThRaw :: n :<= m -> ThRep
fromThRaw = (.thRep)
{-# INLINE fromThRaw #-}

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

data SomeTh
  = forall n m.
  SomeTh
  { lower :: SNat n
  , upper :: SNat m
  , value :: n :<= m
  }

instance Eq SomeTh where
  (==) :: SomeTh -> SomeTh -> Bool
  SomeTh n1 m1 n1m1 == SomeTh n2 m2 n2m2
    | Just Refl <- decSNat n1 n2
    , Just Refl <- decSNat m1 m2 =
        n1m1 == n2m2
    | otherwise = False

deriving stock instance Show SomeTh

instance NFData SomeTh where
  rnf :: SomeTh -> ()
  rnf SomeTh{..} = rnf lower `seq` rnf upper `seq` rnf value

keepAllSomeTh :: SomeSNat -> SomeTh
keepAllSomeTh (SomeSNat bound) =
  SomeTh
    { lower = bound
    , upper = bound
    , value = KeepAll
    }
{-# INLINE keepAllSomeTh #-}

keepOneSomeTh :: SomeTh -> SomeTh
keepOneSomeTh SomeTh{..} =
  SomeTh
    { lower = S lower
    , upper = S upper
    , value = KeepOne value
    }
{-# INLINE keepOneSomeTh #-}

dropOneSomeTh :: SomeTh -> SomeTh
dropOneSomeTh SomeTh{..} =
  SomeTh
    { lower = lower
    , upper = S upper
    , value = DropOne value
    }
{-# INLINE dropOneSomeTh #-}

fromBools :: (Integral i) => i -> [Bool] -> SomeTh
fromBools bound = go
 where
  go [] = keepAllSomeTh (toSomeSNat bound)
  go (False : bools) = keepOneSomeTh (go bools)
  go (True : bools) = dropOneSomeTh (go bools)
{-# SPECIALIZE fromBools :: SNatRep -> [Bool] -> SomeTh #-}

toSomeTh :: (Integral i, Bits bs) => (i, bs) -> SomeTh
toSomeTh (nRep, nmRep) = toSomeThRaw (fromIntegral nRep, copyBits nmRep)
{-# SPECIALIZE toSomeTh :: (SNatRep, ThRep) -> SomeTh #-}

toSomeThRaw :: (SNatRep, ThRep) -> SomeTh
toSomeThRaw (nRep, nmRep)
  | SomeSNat n <- toSomeSNatRaw nRep
  , let dRep = popCount nmRep
  , SomeSNat d <- toSomeSNat dRep
  , let m = n `plus` d
  , let nm = UnsafeTh nmRep =
      SomeTh n m nm
{-# INLINE toSomeThRaw #-}

withSomeTh :: (forall n m. SNat n -> SNat m -> n :<= m -> r) -> SomeTh -> r
withSomeTh action (SomeTh n m nm) = action n m nm
{-# INLINE withSomeTh #-}

-- | Convert a thinning into a bit sequence.
fromSomeTh :: (Integral i, Bits bs) => SomeTh -> (i, bs)
fromSomeTh = bimap fromIntegral thRepToBits . fromSomeThRaw
{-# INLINE fromSomeTh #-}

fromSomeThRaw :: SomeTh -> (SNatRep, ThRep)
fromSomeThRaw = withSomeTh (\n _m nm -> (n.snatRep, nm.thRep))
{-# INLINE fromSomeThRaw #-}

bitsToThRep :: (Bits bs) => bs -> ThRep
bitsToThRep = copyBits
{-# INLINE bitsToThRep #-}

thRepToBits :: (Bits bs) => ThRep -> bs
thRepToBits = copyBits
{-# INLINE thRepToBits #-}

copyBits :: forall bs1 bs2. (Bits bs1, Bits bs2) => bs1 -> bs2
copyBits bs = go 0 (shift zeroBits (bitCount bs)) bs
 where
  go :: Int -> bs2 -> bs1 -> bs2
  go i bs2 bs1
    | bs1 == zeroBits = bs2
    | testBit bs1 0 = go (i + 1) (setBit bs2 i) (shift bs1 (-1))
    | otherwise = go (i + 1) bs2 (shift bs1 (-1))

bitCount :: (Bits bs) => bs -> Int
bitCount bs
  | bs == zeroBits = 0
  | otherwise = 1 + bitCount (shift bs (-1))

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
  thin !t !i = isPos i $
    case t of
      KeepAll -> i
      KeepOne n'm' ->
        case i of
          FZ -> FZ
          FS i' -> FS (thin n'm' i')
      DropOne nm' -> FS (thin nm' i)

  thick :: n :<= m -> Ix m -> Maybe (Ix n)
  thick KeepAll i = Just i
  thick (KeepOne _n'm') FZ = Just FZ
  thick (KeepOne n'm') (FS i') = FS <$> thick n'm' i'
  thick (DropOne _nm') FZ = Nothing
  thick (DropOne nm') (FS i') = thick nm' i'

instance Thin ((:<=) l) where
  thin :: n :<= m -> l :<= n -> l :<= m
  thin nm KeepAll = nm
  thin KeepAll ln = ln
  thin (KeepOne n'm') (KeepOne l'n') = KeepOne (thin n'm' l'n')
  thin (KeepOne n'm') (DropOne ln') = DropOne (thin n'm' ln')
  thin (DropOne nm') ln = DropOne (thin nm' ln)

  thick :: n :<= m -> l :<= m -> Maybe (l :<= n)
  thick KeepAll lm = Just lm
  thick (KeepOne n'm') KeepAll = KeepOne <$> thick n'm' KeepAll
  thick (KeepOne n'm') (KeepOne l'n') = KeepOne <$> thick n'm' l'n'
  thick (KeepOne n'm') (DropOne ln') = DropOne <$> thick n'm' ln'
  thick (DropOne _nm') KeepAll = Nothing
  thick (DropOne _nm') (KeepOne _l'n') = Nothing
  thick (DropOne nm') (DropOne ln') = thick nm' ln'
