{-# LANGUAGE DataKinds #-}
-- | Provides empty typed SQLFragments to force fragment
-- type signature using !::! and !#:! operators.
-- This module is meant to be import as qualified as some
-- names can clashes with standard names.
--
-- >>> :set -XOverloadedStrings
-- >>> :t "a" !::! string 
-- "a" !::! string :: SQLFragment '[String] '[]
module Database.SQLFragment.Proxies where

import Database.SQLFragment.SQLFragment
import Data.Monoid

empty :: SQLFragment '[] '[]
empty = mempty
string = forgetTypes empty :: SQLFragment '[String] '[]
int = forgetTypes empty :: SQLFragment '[Int] '[]
double = forgetTypes empty :: SQLFragment '[Double] '[]


