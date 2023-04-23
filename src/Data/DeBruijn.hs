{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.DeBruijn
  ( DB (Z, S),
    raise,
    inject,
  )
where

import Data.Data (Proxy (..))
import GHC.Num.Natural (Natural, naturalSubUnsafe)
import GHC.TypeLits (natVal)
import GHC.TypeNats (KnownNat, Nat, type (+))

newtype DB (n :: Nat) = DB# Natural

pattern Z :: forall n. DB (1 + n)
pattern Z = DB# 0

pattern S :: forall n. DB n -> DB (1 + n)
pattern S db <- (unsafeDBPred -> db)
  where
    S (DB# index) = DB# (1 + index)

unsafeDBPred :: DB (1 + n) -> DB n
unsafeDBPred (DB# index) = DB# (index `naturalSubUnsafe` 1)

raise :: forall m n. (KnownNat m) => DB n -> DB (m + n)
raise (DB# index) = let m = natVal (Proxy @m) in DB# (fromInteger m + index)

inject :: forall m n. (KnownNat m) => DB n -> DB (m + n)
inject (DB# index) = DB# index
