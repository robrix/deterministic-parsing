module Data.Set.Hybrid
( Set
, singleton
) where

import qualified Data.Set as Set
import qualified Data.Set.Symbolic as Sym

data Set a
  = Set (Set.Set a)
  | Sym (Sym.Set a)

singleton :: a -> Set a
singleton = Set . Set.singleton
