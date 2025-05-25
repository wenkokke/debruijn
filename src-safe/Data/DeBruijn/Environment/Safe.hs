{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.DeBruijn.Environment.Safe (
  Env (Nil, (:>)),
  decEnvLen,
  (!),
) where

import Control.DeepSeq (NFData (..))
import Data.DeBruijn.Index.Safe (Ix (FS, FZ), isPos)
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Nat (type Nat (..))
import Prelude hiding (lookup)

-- | @'Env' n@ is the type of environments with @n@ elements.
type Env :: Nat -> Type -> Type
data Env n a where
  Nil :: Env Z a
  (:>) :: Env n a -> a -> Env (S n) a

deriving stock instance Functor (Env n)

deriving stock instance Foldable (Env n)

deriving stock instance Traversable (Env n)

decEnvLen :: Env n a -> Env m a -> Maybe (n :~: m)
decEnvLen Nil Nil = Just Refl
decEnvLen (xs :> _x) (ys :> _y) = (\Refl -> Refl) <$> decEnvLen xs ys
decEnvLen _ _ = Nothing

instance (NFData a) => NFData (Env n a) where
  rnf :: Env n a -> ()
  rnf Nil = ()
  rnf (xs :> x) = rnf x `seq` rnf xs

instance (Eq a) => Eq (Env n a) where
  (==) :: Env n a -> Env n a -> Bool
  Nil == Nil = True
  xs :> x == ys :> y = x == y && xs == ys

deriving instance (Show a) => Show (Env n a)

(!) :: Env n a -> Ix n -> a
xs ! i = isPos i $ case (xs, i) of
  (_ys :> y, FZ) -> y
  (ys :> _y, FS j) -> ys ! j
