{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.DeBruijn.Index.Safe (
  -- * DeBruijn indices
  Ix (FZ, FS),
  eqIx,
  toInductive,
  fromInductive,
  fromIx,
  fromIxRaw,
  isPos,
  thin,
  thick,
  inject,
  raise,

  -- * Existential Wrapper
  SomeIx (..),
  withSomeIx,
  toSomeIx,
  toSomeIxRaw,
  fromSomeIx,
  fromSomeIxRaw,

  -- * Specialised target for conversion
  IxRep,
) where

import Control.DeepSeq (NFData (..))
import Data.DeBruijn.Index.Fast (IxRep)
import Data.DeBruijn.Index.Fast qualified as Fast
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Nat (type Nat (..), type Pos, type (+))
import Data.Type.Nat.Singleton.Safe (SNat (..), SNatRep, SomeSNat (..), decSNat, fromSNat, fromSNatRaw, plusUnitR, toSomeSNat)
import Text.Printf (printf)

{- $setup
>>> import Data.DeBruijn.Index.Safe.Arbitrary
-}

--------------------------------------------------------------------------------
-- DeBruijn Indexes
--------------------------------------------------------------------------------

-- | @'Ix' n@ is the type of DeBruijn indices less than @n@.
type Ix :: Nat -> Type
data Ix n where
  FZ :: Ix (S n)
  FS :: Ix n -> Ix (S n)

eqIx :: Ix n -> Ix m -> Bool
eqIx FZ FZ = True
eqIx (FS i) (FS j) = eqIx i j
eqIx _ _ = False

instance Eq (Ix n) where
  (==) :: Ix n -> Ix n -> Bool
  (==) = eqIx

instance Show (Ix n) where
  showsPrec :: Int -> Ix n -> ShowS
  showsPrec p =
    showParen (p > 10) . \case
      FZ -> showString "FZ"
      FS n -> showString "FS " . showsPrec 11 n

instance NFData (Ix n) where
  rnf :: Ix n -> ()
  rnf FZ = ()
  rnf (FS i) = rnf i

-- | Convert from the efficient representation 'Fast.Ix' to the safe representation 'Ix'.
toInductive :: Fast.Ix n -> Ix n
toInductive Fast.FZ = FZ
toInductive (Fast.FS i) = FS (toInductive i)

-- | Convert from the safe representation 'Ix' to the efficient representation 'Fast.Ix'.
fromInductive :: Ix n -> Fast.Ix n
fromInductive FZ = Fast.FZ
fromInductive (FS i) = Fast.FS (fromInductive i)

-- | Convert an 'Ix' to 'Word'.
{-# SPECIALIZE fromIx :: Ix n -> Int #-}
fromIx :: (Integral i) => Ix n -> i
fromIx = \case
  FZ -> 0
  FS i -> 1 + fromIx i

fromIxRaw :: Ix n -> Int
fromIxRaw = fromIx
{-# INLINE fromIxRaw #-}

-- | If any value of type @'Ix' n@ exists, @n@ must have a predecessor.
isPos :: Ix n -> ((Pos n) => a) -> a
isPos FZ r = r
isPos (FS _) r = r

-- | Thinning.
thin :: Ix (S n) -> Ix n -> Ix (S n)
thin FZ j = FS j
thin (FS _) FZ = FZ
thin (FS i) (FS j) = FS (thin i j)

-- | Thickening.
thick :: Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick FZ FZ = Nothing
thick FZ (FS j) = Just j
thick (FS i) FZ = isPos i $ Just FZ
thick (FS i) (FS j) = isPos i $ FS <$> thick i j

-- | Inject.
inject :: Ix n -> SNat m -> Ix (n + m)
inject FZ m = case plusUnitR m of Refl -> FZ
inject (FS i) n = FS (inject i n)

-- | Raise.
raise :: SNat n -> Ix m -> Ix (n + m)
raise Z j = j
raise (S n) j = FS (raise n j)

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

-- | An existential wrapper around indexes.
type SomeIx :: Type
data SomeIx = forall (n :: Nat). SomeIx
  { bound :: !(SNat n)
  , index :: !(Ix n)
  }

instance Eq SomeIx where
  (==) :: SomeIx -> SomeIx -> Bool
  SomeIx n i == SomeIx m j
    | Just Refl <- decSNat n m = eqIx i j
    | otherwise = False

deriving instance Show SomeIx

instance NFData SomeIx where
  rnf :: SomeIx -> ()
  rnf SomeIx{..} = rnf bound `seq` rnf index

withSomeIx :: (forall n. SNat n -> Ix n -> a) -> SomeIx -> a
withSomeIx action (SomeIx n i) = action n i

{-| @'toSomeIx' n@ constructs the index @n@ at type @'Ix' n@ from the number @n@.

prop> toSomeIx (fromSomeIx i) == i
-}
toSomeIx :: (Integral i) => (i, i) -> SomeIx
toSomeIx (bound, index)
  | index < 0 = error $ printf "index cannot contain negative value, found index %d" (toInteger index)
  | bound <= index = error "bound must be larger than index, found bound %d and index %d" (toInteger bound) (toInteger index)
  | bound >= 1, index == 0, SomeSNat n <- toSomeSNat (pred bound) = SomeIx (S n) FZ
  | SomeIx n i <- toSomeIx (pred bound, pred index) = SomeIx (S n) (FS i)

{-| @'toSomeIxRaw' n@ constructs the index @n@ at type @'Ix' n@ from the 'IxRep' @n@.

prop> toSomeIxRaw (fromSomeIxRaw i) == i
-}
toSomeIxRaw :: (SNatRep, IxRep) -> SomeIx
toSomeIxRaw = toSomeIx

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped index.
fromSomeIx :: (Integral i) => SomeIx -> (i, i)
fromSomeIx = withSomeIx (\n i -> (fromSNat n, fromIx i))

-- | @'fromSomeSNat' n@ returns the 'IxRep' representation of the wrapped index.
fromSomeIxRaw :: SomeIx -> (SNatRep, IxRep)
fromSomeIxRaw = withSomeIx (\n i -> (fromSNatRaw n, fromIxRaw i))
