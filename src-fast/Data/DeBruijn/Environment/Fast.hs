{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.DeBruijn.Environment.Fast (
  -- * Environments
  Env (Nil, (:>)),
  (!),

  -- * Fast
  EnvRep,
  Env (UnsafeEnv, envRep),
) where

import Data.DeBruijn.Index.Fast (Ix (ixRep), ixRepToInt)
import Data.Kind (Type)
import Data.Type.Nat (Nat (..), Pos, Pred)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Conditional Imports
--------------------------------------------------------------------------------

#if defined(ENV_AS_SEQ)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Maybe (fromJust)
#elif defined(ENV_AS_SKEW_LIST)
import Data.SkewList.Strict (SkewList)
import Data.SkewList.Strict qualified as SkewList
#endif

--------------------------------------------------------------------------------
-- Environment Representation
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Environment Representation: Finger Tree
#if defined(ENV_AS_SEQ)

type EnvRep a = Seq a

mkNilRep :: EnvRep a
mkNilRep = Seq.empty
{-# INLINE mkNilRep #-}

mkSnocRep :: EnvRep a -> a -> EnvRep a
mkSnocRep xs x = xs Seq.:|> x
{-# INLINE mkSnocRep #-}

elEnvRep :: b -> (EnvRep a -> a -> b) -> EnvRep a -> b
elEnvRep ifNil ifSnoc = \case
  Seq.Empty -> ifNil
  xs Seq.:|> x -> ifSnoc xs x
{-# INLINE elEnvRep #-}

lookupRep :: Int -> EnvRep a -> a
lookupRep = (fromJust .) . Seq.lookup
{-# INLINE lookupRep #-}
{-# ANN lookupRep ("HLint: ignore Avoid partial function" :: String) #-}

--------------------------------------------------------------------------------
-- Environment Representation: Skew List
#elif defined(ENV_AS_SKEW_LIST)

type EnvRep a = SkewList a

mkNilRep :: EnvRep a
mkNilRep = SkewList.Nil
{-# INLINE mkNilRep #-}

mkSnocRep :: EnvRep a -> a -> EnvRep a
mkSnocRep xs x = SkewList.Cons x xs
{-# INLINE mkSnocRep #-}

elEnvRep :: b -> (EnvRep a -> a -> b) -> EnvRep a -> b
elEnvRep ifNil ifSnoc = \case
  SkewList.Nil -> ifNil
  SkewList.Cons x xs -> ifSnoc xs x
{-# INLINE elEnvRep #-}

lookupRep :: Int -> EnvRep a -> a
lookupRep = flip (SkewList.!)
{-# INLINE lookupRep #-}
{-# ANN lookupRep ("HLint: ignore Avoid partial function" :: String) #-}

--------------------------------------------------------------------------------
-- Environment Representation: None
#else
#error "cpp: define one of [ENV_AS_SEQ, ENV_AS_SKEW_LIST]"
#endif

--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------

type Env :: Nat -> Type -> Type
newtype Env n a = UnsafeEnv {envRep :: EnvRep a}

type role Env nominal representational

deriving stock instance Functor (Env n)

deriving stock instance Foldable (Env n)

deriving stock instance Traversable (Env n)

mkNil :: Env Z a
mkNil = UnsafeEnv mkNilRep
{-# INLINE mkNil #-}

mkSnoc :: Env n a -> a -> Env (S n) a
mkSnoc = (UnsafeEnv .) . mkSnocRep . (.envRep)
{-# INLINE mkSnoc #-}

elEnv :: b -> (Env (Pred n) a -> a -> b) -> Env n a -> b
elEnv ifNil ifSnoc = elEnvRep ifNil (ifSnoc . UnsafeEnv) . (.envRep)
{-# INLINE elEnv #-}

type EnvF :: (Nat -> Type -> Type) -> Nat -> Type -> Type
data EnvF env n a where
  NilF :: EnvF env Z a
  SnocF :: env n a -> a -> EnvF env (S n) a

projectEnv :: Env n a -> EnvF Env n a
projectEnv = elEnv (unsafeCoerce NilF) (unsafeCoerce . SnocF)
{-# INLINE projectEnv #-}

embedEnv :: EnvF Env n a -> Env n a
embedEnv = \case
  NilF -> mkNil
  SnocF xs x -> mkSnoc xs x
{-# INLINE embedEnv #-}

pattern Nil :: () => (n ~ Z) => Env n a
pattern Nil <- (projectEnv -> NilF) where Nil = embedEnv NilF
{-# INLINE Nil #-}

pattern (:>) :: () => (Pos n) => Env (Pred n) a -> a -> Env n a
pattern (:>) xs x <- (projectEnv -> SnocF xs x) where (:>) xs x = embedEnv (SnocF xs x)
{-# INLINE (:>) #-}

{-# COMPLETE Nil, (:>) #-}

(!) :: Env n a -> Ix n -> a
xs ! i = lookupRep (ixRepToInt i.ixRep) xs.envRep
