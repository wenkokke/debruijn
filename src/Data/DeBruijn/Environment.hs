{-# LANGUAGE CPP #-}

module Data.DeBruijn.Environment (
  -- * Environments
  Env (Nil, (:>)),
  (!),
) where

#ifdef EXPORT_SAFE_API
import Data.DeBruijn.Environment.Fast (
  Env (Nil, (:>)),
  (!),
 )
#else
import Data.DeBruijn.Environment.Fast (
  Env (Nil, (:>)),
  (!),
 )
#endif
