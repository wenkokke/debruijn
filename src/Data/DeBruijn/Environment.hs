module Data.DeBruijn.Environment (
  -- * Environments
  Env (Nil, (:>)),
  lookup,
) where

import Data.DeBruijn.Environment.Unsafe (
  Env (Nil, (:>)),
  lookup,
 )
