{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.Type.Nat.Singleton.Fast (
  -- * Natural Number Singletons
  SNat (Z, S),
  fromSNat,
  fromSNatRaw,
  decSNat,

  -- * Existential Wrapper
  SomeSNat (..),
  withSomeSNat,
  toSomeSNat,
  toSomeSNatRaw,
  fromSomeSNat,
  fromSomeSNatRaw,

  -- * Laws
  plusUnitL,
  plusUnitR,
  plusCommS,
  plusComm,
  plusAssoc,

  -- * Linking Type-Level and Value-Level
  KnownNat (..),
  withKnownNat,

  -- * Fast
  SNatRep,
  SNat (UnsafeSNat, snatRep),
) where

import Control.DeepSeq (NFData (..))
import Control.Exception (assert)
import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Nat (Nat (..), Pos, Pred, type (+))
import GHC.TypeLits qualified as GHC
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

{- $setup
>>> import Data.Type.Nat.Singleton.Fast.Arbitrary
-}

--------------------------------------------------------------------------------
-- Natural Number Singleton Representation
--------------------------------------------------------------------------------

type SNatRep = Int

isValidSNatRep :: SNatRep -> Bool
isValidSNatRep = (>= 0)

mkZRep :: SNatRep
mkZRep = 0
{-# INLINE mkZRep #-}

mkSRep :: SNatRep -> SNatRep
mkSRep = (1 +)
{-# INLINE mkSRep #-}

unSRep :: SNatRep -> SNatRep
unSRep = subtract 1
{-# INLINE unSRep #-}

elSNatRep :: a -> (SNatRep -> a) -> SNatRep -> a
elSNatRep ifZ ifS n =
  assert (isValidSNatRep n) $
    if n == mkZRep
      then ifZ
      else ifS (unSRep n)
{-# INLINE elSNatRep #-}

--------------------------------------------------------------------------------
-- Natural Number Singletons
--------------------------------------------------------------------------------

-- | @'SNat' n@ is the singleton type for natural numbers.
type SNat :: Nat -> Type
newtype SNat n = UnsafeSNat {snatRep :: SNatRep}

instance NFData (SNat n) where
  rnf :: SNat n -> ()
  rnf (UnsafeSNat n) = rnf n

type role SNat nominal

mkZ :: SNat Z
mkZ = UnsafeSNat mkZRep
{-# INLINE mkZ #-}

mkS :: SNat n -> SNat (S n)
mkS = UnsafeSNat . mkSRep . (.snatRep)
{-# INLINE mkS #-}

-- | @'fromSNat' n@ returns the numeric representation of 'SNat n'.
fromSNat :: (Integral i) => SNat n -> i
fromSNat = fromInteger . toInteger . (.snatRep)

-- | @'fromSNatRaw' n@ returns the raw underlying representation of 'SNat n'.
fromSNatRaw :: SNat n -> SNatRep
fromSNatRaw = (.snatRep)

instance Show (SNat n) where
  showsPrec :: Int -> SNat n -> ShowS
  showsPrec p = \case
    Z -> showString "Z"
    S n -> showParen (p > 10) $ showString "S " . showsPrec 11 n

-- | @'SNatF'@ is the base functor of @'SNat'@.
data SNatF (snat :: Nat -> Type) (n :: Nat) where
  ZF :: SNatF snat Z
  SF :: !(snat n) -> SNatF snat (S n)

projectSNat :: SNat n -> SNatF SNat n
projectSNat =
  elSNatRep (unsafeCoerce ZF) (unsafeCoerce . SF . UnsafeSNat) . (.snatRep)
{-# INLINE projectSNat #-}

embedSNat :: SNatF SNat n -> SNat n
embedSNat = \case
  ZF -> mkZ
  SF n -> mkS n
{-# INLINE embedSNat #-}

pattern Z :: () => (n ~ Z) => SNat n
pattern Z <- (projectSNat -> ZF) where Z = embedSNat ZF
{-# INLINE Z #-}

pattern S :: () => (Pos n) => SNat (Pred n) -> SNat n
pattern S n <- (projectSNat -> SF n) where S n = embedSNat (SF n)
{-# INLINE S #-}

{-# COMPLETE Z, S #-}

-- | Decidable equality for natural number singletons.
decSNat :: SNat n -> SNat m -> Maybe (n :~: m)
decSNat n m =
  if n.snatRep == m.snatRep
    then Just (unsafeCoerce Refl)
    else Nothing

instance Eq (SNat n) where
  (==) :: SNat n -> SNat n -> Bool
  m == n = isJust (decSNat m n)

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

-- | An existential wrapper around natural number singletons.
type SomeSNat :: Type
data SomeSNat = forall (n :: Nat). SomeSNat !(SNat n)

instance NFData SomeSNat where
  rnf :: SomeSNat -> ()
  rnf (SomeSNat n) = rnf n

deriving instance Show SomeSNat

instance Eq SomeSNat where
  (==) :: SomeSNat -> SomeSNat -> Bool
  SomeSNat m == SomeSNat n = isJust (decSNat m n)

-- | Evaluate a term with access to the underlying @'SNat'@.
withSomeSNat :: (forall n. SNat n -> a) -> SomeSNat -> a
withSomeSNat action (SomeSNat n) = action n

{-| @'toSomeSNat' n@ constructs the singleton @'SNat' n@.

prop> toSomeSNat (fromSomeSNat n) == n
-}
toSomeSNat :: (Integral i) => i -> SomeSNat
toSomeSNat r
  | r < 0 = error $ printf "cannot convert %d to natural number singleton" (toInteger r)
  | otherwise = SomeSNat (UnsafeSNat (fromIntegral r))

{-| @'toSomeSNat' n@ constructs the singleton @'SNat' n@.

prop> toSomeSNatRaw (fromSomeSNatRaw n) == n
-}
toSomeSNatRaw :: SNatRep -> SomeSNat
toSomeSNatRaw r
  | r < 0 = error $ printf "cannot convert %d to natural number singleton"
  | otherwise = SomeSNat (UnsafeSNat r)

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped singleton.
fromSomeSNat :: (Integral i) => SomeSNat -> i
fromSomeSNat = withSomeSNat fromSNat

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped singleton.
fromSomeSNatRaw :: SomeSNat -> SNatRep
fromSomeSNatRaw (SomeSNat (UnsafeSNat r)) = r

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

plusUnitL :: Proxy n -> Z + n :~: n
plusUnitL _ = Refl

plusUnitR :: SNat n -> n + Z :~: n
plusUnitR _ = unsafeCoerce Refl

plusCommS :: SNat n -> Proxy m -> S (n + m) :~: n + S m
plusCommS _ _ = unsafeCoerce Refl

plusComm :: SNat n -> SNat m -> n + m :~: m + n
plusComm _ _ = unsafeCoerce Refl

plusAssoc :: SNat n -> Proxy m -> Proxy l -> (n + m) + l :~: n + (m + l)
plusAssoc _ _ _ = unsafeCoerce Refl

--------------------------------------------------------------------------------
-- Linking Type-Level and Value-Level
--------------------------------------------------------------------------------

type FromNat :: Nat -> GHC.Nat
type family FromNat n where
  FromNat Z = 0
  FromNat (S n) = FromNat n GHC.+ 1

type KnownNat :: Nat -> Constraint
class KnownNat n where
  natSing :: SNat n

instance KnownNat Z where
  natSing :: SNat Z
  natSing = Z

instance (KnownNat n) => KnownNat (S n) where
  natSing :: SNat (S n)
  natSing = S natSing

data Dict (c :: Constraint) :: Type where
  Dict :: (c) => Dict c

data FakeKnownNat n = FakeKnownNat (SNat n)
{-# ANN FakeKnownNat ("HLint: ignore Use newtype instead of data" :: String) #-}

withKnownNat :: SNat n -> ((KnownNat n) => r) -> r
withKnownNat n action = case knownNat n of Dict -> action
 where
  knownNat :: SNat n -> Dict (KnownNat n)
  knownNat = unsafeCoerce . FakeKnownNat
