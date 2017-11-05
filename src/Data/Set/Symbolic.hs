module Data.Set.Symbolic
( Set
, intersection
) where

import Data.Semigroup

data Set a = Set (a -> Bool)

intersection :: Set a -> Set a -> Set a
intersection (Set p1) (Set p2) = Set ((&&) <$> p1 <*> p2)

instance Semigroup (Set a) where
  Set p1 <> Set p2 = Set ((||) <$> p1 <*> p2)

instance Monoid (Set a) where
  mempty = Set (const False)
  mappend = (<>)
