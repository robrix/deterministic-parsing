module Data.Set.Symbolic
( Set
, fromList
, fromPredicate
, intersection
, member
) where

import Data.Semigroup
import qualified Data.Set as Set

newtype Set a = Set (a -> Bool)

fromList :: Ord a => [a] -> Set a
fromList list = Set (`Set.member` Set.fromList list)

fromPredicate :: (a -> Bool) -> Set a
fromPredicate = Set

intersection :: Set a -> Set a -> Set a
intersection (Set p1) (Set p2) = Set ((&&) <$> p1 <*> p2)

member :: a -> Set a -> Bool
member a (Set p) = p a


instance Semigroup (Set a) where
  Set p1 <> Set p2 = Set ((||) <$> p1 <*> p2)

instance Monoid (Set a) where
  mempty = Set (const False)
  mappend = (<>)
