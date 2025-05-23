{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.DeBruijn.Index.Unsafe (
  -- * DeBruijn Indexes
  Ix (FZ, FS),
  eqIx,
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

  -- * Unsafe
  Ix (UnsafeIx, ixRep),
) where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat (Nat (..), Pos, Pred, type (+))
import Data.Type.Nat.Singleton.Unsafe (SNat (..), decSNat)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

{- $setup
>>> import Data.DeBruijn.Index.Unsafe.Arbitrary
-}

--------------------------------------------------------------------------------
-- DeBruijn Index Representation
--------------------------------------------------------------------------------

#define IxRep Int

mkFZRep :: IxRep
mkFZRep = 0
{-# INLINE mkFZRep #-}

mkFSRep :: IxRep -> IxRep
mkFSRep = (1 +)
{-# INLINE mkFSRep #-}

unFSRep :: IxRep -> IxRep
unFSRep = subtract 1
{-# INLINE unFSRep #-}

elIxRep :: a -> (IxRep -> a) -> IxRep -> a
elIxRep ifZ ifS i =
  if i == mkFZRep then ifZ else ifS (unFSRep i)
{-# INLINE elIxRep #-}

thinRep :: IxRep -> IxRep -> IxRep
thinRep i j
  | i <= j = mkFSRep j
  | otherwise = j

thickRep :: IxRep -> IxRep -> Maybe IxRep
thickRep i j = case i `compare` j of
  LT -> Just (unFSRep j)
  EQ -> Nothing
  GT -> Just j

--------------------------------------------------------------------------------
-- DeBruijn Indexes
--------------------------------------------------------------------------------

-- | @'Ix' n@ is the type of DeBruijn indices less than @n@.
type Ix :: Nat -> Type
newtype Ix n = UnsafeIx {ixRep :: IxRep}

type role Ix nominal

eqIx :: Ix n -> Ix m -> Bool
eqIx i j = fromIxRaw i == fromIxRaw j

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
  rnf (UnsafeIx u) = rnf u

mkFZ :: Ix (S n)
mkFZ = UnsafeIx mkFZRep
{-# INLINE mkFZ #-}

mkFS :: Ix n -> Ix (S n)
mkFS = UnsafeIx . mkFSRep . (.ixRep)
{-# INLINE mkFS #-}

elIx :: a -> (Ix (Pred n) -> a) -> Ix n -> a
elIx ifFZ ifFS = elIxRep ifFZ (ifFS . UnsafeIx) . (.ixRep)
{-# INLINE elIx #-}

-- | @'fromSNat' n@ returns the numeric representation of 'SNat n'.
fromIx :: (Integral i) => Ix n -> i
fromIx = fromInteger . toInteger . (.ixRep)
{-# INLINE fromIx #-}

-- | @'fromIxRaw' n@ returns the raw numeric representation of 'SNat n'.
fromIxRaw :: Ix n -> IxRep
fromIxRaw = (.ixRep)
{-# INLINE fromIxRaw #-}

-- | @'IxF'@ is the base functor of @'Ix'@.
data IxF (ix :: Nat -> Type) (n :: Nat) :: Type where
  FZF :: IxF ix (S m)
  FSF :: !(ix m) -> IxF ix (S m)

projectIx :: Ix n -> IxF Ix n
projectIx = elIx (unsafeCoerce FZF) (unsafeCoerce . FSF)
{-# INLINE projectIx #-}

embedIx :: IxF Ix n -> Ix n
embedIx = \case
  FZF -> mkFZ
  FSF i -> mkFS i
{-# INLINE embedIx #-}

-- NOTE:
--   Type signatures for pattern synonyms are weird, see:
--   https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html#typing-of-pattern-synonyms

pattern FZ :: () => (Pos n) => Ix n
pattern FZ <- (projectIx -> FZF) where FZ = embedIx FZF
{-# INLINE FZ #-}

pattern FS :: () => (Pos n) => Ix (Pred n) -> Ix n
pattern FS i <- (projectIx -> FSF i) where FS i = embedIx (FSF i)
{-# INLINE FS #-}

{-# COMPLETE FZ, FS #-}

-- | If any value of type @'Ix' n@ exists, @n@ must have a predecessor.
isPos :: Ix n -> ((Pos n) => a) -> a
isPos FZ r = r
isPos (FS _) r = r

-- | Thinning.
thin :: Ix (S n) -> Ix n -> Ix (S n)
thin i j = UnsafeIx (thinRep i.ixRep j.ixRep)

-- | Thickening.
thick :: Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick i j = UnsafeIx <$> thickRep i.ixRep j.ixRep

-- | Inject.
inject :: Proxy n -> Ix m -> Ix (n + m)
inject _ j = UnsafeIx j.ixRep

-- | Raise.
raise :: SNat n -> Ix m -> Ix (n + m)
raise n j = UnsafeIx (n.snatRep + j.ixRep)

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

-- | An existential wrapper around indexes.
type SomeIx :: Type
data SomeIx = forall (n :: Nat). SomeIx
  { bound :: {-# UNPACK #-} !(SNat n)
  , index :: {-# UNPACK #-} !(Ix n)
  }

instance NFData SomeIx where
  rnf :: SomeIx -> ()
  rnf (SomeIx n i) = rnf n `seq` rnf i

instance Eq SomeIx where
  (==) :: SomeIx -> SomeIx -> Bool
  SomeIx n i == SomeIx m j
    | Just Refl <- decSNat n m = eqIx i j
    | otherwise = False

deriving instance Show SomeIx

withSomeIx :: (forall n. SNat n -> Ix n -> a) -> SomeIx -> a
withSomeIx action (SomeIx n i) = action n i

{-| @'toSomeIx' n@ constructs the index @n@ at type @'Ix' n@ from the number @n@.

prop> toSomeIx (fromSomeIx i) == i
-}
toSomeIx :: (Integral i) => (i, i) -> SomeIx
toSomeIx = toSomeIxRaw . bimap fromIntegral fromIntegral

{-| @'toSomeIxRaw' n@ constructs the index @n@ at type @'Ix' n@ from the 'Int' @n@.

prop> toSomeIxRaw (fromSomeIxRaw i) == i
-}
toSomeIxRaw :: (IxRep, IxRep) -> SomeIx
toSomeIxRaw (n, i)
  | i < 0 = error $ printf "index cannot contain negative value, found index %d" i
  | n <= i = error $ printf "bound must be larger than index, found bound %d and index %d" n i
  | otherwise = SomeIx (UnsafeSNat n) (UnsafeIx i)

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped index.
fromSomeIx :: (Integral i) => SomeIx -> (i, i)
fromSomeIx = bimap fromIntegral fromIntegral . fromSomeIxRaw

-- | @'fromSomeSNat' n@ returns the 'Int' representation of the wrapped index.
fromSomeIxRaw :: SomeIx -> (IxRep, IxRep)
fromSomeIxRaw (SomeIx (UnsafeSNat bound) (UnsafeIx index)) = (bound, index)
