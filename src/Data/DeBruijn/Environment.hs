module Data.DeBruijn.Environment (
  -- * Environments
  Env (Nil, (:>)),
  (!),
) where

import Data.DeBruijn.Environment.Fast (
  Env (Nil, (:>)),
  (!),
 )
