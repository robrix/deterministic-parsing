module Data.Set.Hybrid
( Set
, singleton
, fromPredicate
) where

import qualified Data.Set as Set
import qualified Data.Set.Symbolic as Sym

data Set a
  = Set (Set.Set a)
  | Sym (Sym.Set a)

singleton :: a -> Set a
singleton = Set . Set.singleton

fromPredicate :: (a -> Bool) -> Set a
fromPredicate = Sym . Sym.fromPredicate
