module Data.Set.Hybrid
( Set
, singleton
, fromPredicate
, member
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

member :: Ord a => a -> Set a -> Bool
member a (Set s) = Set.member a s
member a (Sym s) = Sym.member a s
