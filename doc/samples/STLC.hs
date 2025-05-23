{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module STLC where

-- import Data.DeBruijn.Environment (Env, (!))
import Data.DeBruijn.Index (Ix)
import Data.DeBruijn.Thinning (type (:<=) (..), Thin (..))
import Data.Kind (Type)
import Data.Type.Nat (Nat (..))
import Data.Type.Nat.Singleton ()

type Tm :: Nat -> Type
data Tm n where
  Var :: Ix n -> Tm n
  Lam :: Tm (S n) -> Tm n
  App :: Tm n -> Tm n -> Tm n

instance Thin Tm where
  thin :: n :<= m -> Tm n -> Tm m
  thin nm = \case
    Var ix -> Var (thin nm ix)
    Lam body -> Lam (thin (Keep nm) body)
    App fun arg -> App (thin nm fun) (thin nm arg)

  thick :: n :<= m -> Tm m -> Maybe (Tm n)
  thick nm = \case
    Var ix -> Var <$> thick nm ix
    Lam body -> Lam <$> thick (Keep nm) body
    App fun arg -> App <$> thick nm fun <*> thick nm arg

-- exts :: Env n (Tm m) -> Env (S n) (Tm (S m))
-- exts env = fmap (Drop (keepAll _)) env :> Var FZ

-- subst :: Env n (Tm m) -> Tm n -> Tm m
-- subst env = \case
--   Var ix -> env ! ix
--   Lam body -> Lam (subst (_ env :> Var FZ) body)
--   App fun arg -> App (subst env fun) (subst env arg)
