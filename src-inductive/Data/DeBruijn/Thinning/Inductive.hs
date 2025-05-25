{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Data.DeBruijn.Thinning.Inductive (
  -- * Thinnings
  (:<=) (KeepAll, KeepOne, DropOne),
  toInductive,
  fromInductive,
  dropAll,
  toBools,

  -- * Existential Wrapper
  SomeTh (..),
  fromBools,
  fromBits,
  fromBitsRaw,

  -- * The action of thinnings on 'Nat'-indexed types
  Thin (..),
) where

import Control.DeepSeq (NFData (..))
import Data.Bits (Bits (..))
import Data.DeBruijn.Index.Inductive (Ix (..), isPos)
import Data.DeBruijn.Thinning.Unsafe qualified as Unsafe
import Data.Kind (Constraint, Type)
import Data.Type.Nat (Nat (..), Pos, Pred)
import Data.Type.Nat.Singleton.Inductive (SNat (..), SomeSNat (..))

--------------------------------------------------------------------------------
-- Thinnings
--------------------------------------------------------------------------------

-- TODO:
-- This does not work, because it stores superfluous information, i.e., we're not
-- supposed to be able to tell the difference between @KeepAll@ and @KeepOne KeepAll@.

-- | @n ':<=' m@ is the type of thinnings from @m@ to @n@.
type (:<=) :: Nat -> Nat -> Type
data (:<=) n m where
  KeepAll :: n :<= n
  KeepOne_ :: n :<= m -> S n :<= S m
  DropOne :: n :<= m -> n :<= S m

keepOne :: n :<= m -> S n :<= S m
keepOne KeepAll = KeepAll
keepOne n'm' = KeepOne_ n'm'

pattern KeepOne :: () => (Pos n, Pos m) => Pred n :<= Pred m -> n :<= m
pattern KeepOne n'm' <- KeepOne_ n'm' where KeepOne n'm' = keepOne n'm'

{-# COMPLETE KeepAll, KeepOne, DropOne #-}

deriving stock instance Eq (n :<= m)

instance Show (n :<= m) where
  showsPrec :: Int -> n :<= m -> ShowS
  showsPrec p =
    showParen (p > 10) . \case
      KeepAll -> showString "KeepAll"
      KeepOne n'm' -> showString "KeepOne " . showsPrec 11 n'm'
      DropOne nm' -> showString "DropOne " . showsPrec 11 nm'

instance NFData (n :<= m) where
  rnf :: n :<= m -> ()
  rnf KeepAll = ()
  rnf (KeepOne n'm') = rnf n'm'
  rnf (DropOne nm') = rnf nm'

-- | Convert from the efficient representation 'Unsafe.:<=' to the inductive representation ':<='.
toInductive :: n Unsafe.:<= m -> n :<= m
toInductive = \case
  Unsafe.KeepAll -> KeepAll
  Unsafe.KeepOne n'm' -> KeepOne (toInductive n'm')
  Unsafe.DropOne nm' -> DropOne (toInductive nm')

-- | Convert from the inductive representation ':<=' to the efficient representation 'Unsafe.:<='.
fromInductive :: n :<= m -> n Unsafe.:<= m
fromInductive = \case
  KeepAll -> Unsafe.KeepAll
  KeepOne n'm' -> Unsafe.KeepOne (fromInductive n'm')
  DropOne nm' -> Unsafe.DropOne (fromInductive nm')

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

keepOneSomeTh :: SomeTh -> SomeTh
keepOneSomeTh SomeTh{..} =
  SomeTh
    { lower = S lower
    , upper = S upper
    , value = KeepOne value
    }

dropOneSomeTh :: SomeTh -> SomeTh
dropOneSomeTh SomeTh{..} =
  SomeTh
    { lower = lower
    , upper = S upper
    , value = DropOne value
    }

fromBools :: SomeSNat -> [Bool] -> SomeTh
fromBools bound = go
 where
  go [] = keepAllSomeTh bound
  go (False : bools) = keepOneSomeTh (go bools)
  go (True : bools) = dropOneSomeTh (go bools)

fromBits :: (Bits bs) => SomeSNat -> bs -> SomeTh
fromBits bound = go
 where
  go bits
    | bits == zeroBits = keepAllSomeTh bound
    | testBit bits 0 = dropOneSomeTh (go (shift bits (-1)))
    | otherwise = keepOneSomeTh (go (shift bits (-1)))
{-# SPECIALIZE fromBits :: SomeSNat -> Integer -> SomeTh #-}

fromBitsRaw :: SomeSNat -> Integer -> SomeTh
fromBitsRaw = fromBits

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
