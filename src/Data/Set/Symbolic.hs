module Data.Set.Symbolic
( Set
) where

import Data.Semigroup

data Set a = Set (a -> Bool)

instance Semigroup (Set a) where
  Set p1 <> Set p2 = Set ((||) <$> p1 <*> p2)
