{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.DeBruijn.Environment.Unsafe (
  -- * Environments
  Env (Nil, (:>)),
  lookup,

  -- * Unsafe
  Env (UnsafeEnv, envRep),
) where

import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Type.Nat (Nat (..), Pos, Pred)
import Unsafe.Coerce (unsafeCoerce)
import Data.DeBruijn.Index.Unsafe (Ix (ixRep))
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- Environment Representation
--------------------------------------------------------------------------------

type EnvRep = Seq

mkNilRep :: EnvRep a
mkNilRep = Seq.empty
{-# INLINE mkNilRep #-}

mkSnocRep :: EnvRep a -> a -> EnvRep a
mkSnocRep xs x = xs Seq.:|> x
{-# INLINE mkSnocRep #-}

elEnvRep :: b -> (EnvRep a -> a -> b) -> EnvRep a -> b
elEnvRep ifNil ifSnoc r = case Seq.viewr r of
  Seq.EmptyR -> ifNil
  xs Seq.:> x -> ifSnoc xs x
{-# INLINE elEnvRep #-}

--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------

type Env :: Nat -> Type -> Type
newtype Env n a = UnsafeEnv {envRep :: EnvRep a}

type role Env nominal representational

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

lookup :: Ix n -> Env n a -> a
lookup i xs = fromMaybe err (Seq.lookup i.ixRep xs.envRep)
  where
    err = error "Oh, how the illusion of safety is shattered!"

