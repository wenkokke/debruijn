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

module Data.DeBruijn.Index.Fast (
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

  -- * Fast
  IxRep,
  intToIxRep,
  ixRepToInt,
  snatRepToIxRep,
  ixRepToSNatRep,
  Ix (UnsafeIx, ixRep),
) where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat (Nat (..), Pos, Pred, type (+))
import Data.Type.Nat.Singleton.Fast (SNat (..), SNatRep, decSNat)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

#if defined(IX_AS_WORD8) || defined(SNAT_AS_WORD8)
import Control.Exception (ArithException (Overflow, Underflow), throw)
import Data.Word (Word8)
#endif

{- $setup
>>> import Data.DeBruijn.Index.Fast.Arbitrary
-}

--------------------------------------------------------------------------------
-- DeBruijn Index Representation
--------------------------------------------------------------------------------

#if defined(IX_AS_WORD8)
type IxRep = Word8
#elif defined(IX_AS_INT)
type IxRep = Int
#elif !defined(__HLINT__)
#error "cpp: define one of [IX_AS_WORD8, IX_AS_INT]"
#endif

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
inject :: Ix n -> SNat m -> Ix (n + m)
inject i _m = UnsafeIx i.ixRep

-- | Raise.
raise :: SNat n -> Ix m -> Ix (n + m)
raise n j = UnsafeIx (snatRepToIxRep n.snatRep + j.ixRep)

-- | Convert an 'IxRep' to an 'Int'.
intToIxRep :: Int -> IxRep
#ifdef IX_AS_WORD8
-- TODO: Make this safe.
intToIxRep int
  | int < 0 = throw Underflow
  | int > fromIntegral (maxBound @Word8) = throw Overflow
  | otherwise = fromIntegral @Int @Word8 int
{-# INLINE intToIxRep #-}
#else
intToIxRep = id @Int
{-# INLINE intToIxRep #-}
#endif

-- | Convert an 'IxRep' to an 'Int'.
ixRepToInt :: IxRep -> Int
#ifdef IX_AS_WORD8
ixRepToInt = fromIntegral @Word8 @Int
{-# INLINE ixRepToInt #-}
#else
ixRepToInt = id @Int
{-# INLINE ixRepToInt #-}
#endif

-- | Convert an 'SNatRep' to an 'IxRep'.
snatRepToIxRep :: SNatRep -> IxRep
#ifdef SNAT_AS_WORD8
#ifdef IX_AS_WORD8
snatRepToIxRep = id @Word8
{-# INLINE snatRepToIxRep #-}
#else
snatRepToIxRep = fromIntegral @Word8 @Int
{-# INLINE snatRepToIxRep #-}
#endif
#else
#ifdef IX_AS_WORD8
-- Int -> Word8
snatRepToIxRep snatRep
  | snatRep < 0 = throw Underflow
  | snatRep > fromIntegral (maxBound @Word8) = throw Overflow
  | otherwise = fromIntegral snatRep
#else
snatRepToIxRep = id @Int
{-# INLINE snatRepToIxRep #-}
#endif
#endif

-- | Convert an 'IxRep' to an 'SNatRep'.
ixRepToSNatRep :: IxRep -> SNatRep
#ifdef SNAT_AS_WORD8
#ifdef IX_AS_WORD8
ixRepToSNatRep = id @Word8
{-# INLINE ixRepToSNatRep #-}
#else
ixRepToSNatRep ixRep
  | ixRep < 0 = throw Underflow
  | ixRep > fromIntegral (maxBound @Word8) = throw Overflow
  | otherwise = fromIntegral ixRep
{-# INLINE ixRepToSNatRep #-}
#endif
#else
#ifdef IX_AS_WORD8
ixRepToSNatRep = fromIntegral @Int @Word8
#else
ixRepToSNatRep = id @Int
{-# INLINE ixRepToSNatRep #-}
#endif
#endif

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
toSomeIx :: (Integral n, Integral i) => (n, i) -> SomeIx
toSomeIx = toSomeIxRaw . bimap fromIntegral fromIntegral

{-| @'toSomeIxRaw' n@ constructs the index @n@ at type @'Ix' n@ from the 'Int' @n@.

prop> toSomeIxRaw (fromSomeIxRaw i) == i
-}
toSomeIxRaw :: (SNatRep, IxRep) -> SomeIx
toSomeIxRaw (n, i)
  | i < 0 = error $ printf "index cannot contain negative value, found index %d" i
  | snatRepToIxRep n <= i = error $ printf "bound must be larger than index, found bound %d and index %d" n i
  | otherwise = SomeIx (UnsafeSNat n) (UnsafeIx i)

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped index.
fromSomeIx :: (Integral n, Integral i) => SomeIx -> (n, i)
fromSomeIx = bimap fromIntegral fromIntegral . fromSomeIxRaw

-- | @'fromSomeSNat' n@ returns the 'Int' representation of the wrapped index.
fromSomeIxRaw :: SomeIx -> (SNatRep, IxRep)
fromSomeIxRaw (SomeIx (UnsafeSNat bound) (UnsafeIx index)) = (bound, index)
