module Data.Predicate
( Predicate
, fromList
, fromPredicate
, singleton
, intersection
, complement
, member
) where

import qualified Data.Set as Set

newtype Predicate a = Predicate (a -> Bool)

fromList :: Ord a => [a] -> Predicate a
fromList list = Predicate (`Set.member` set)
  where set = Set.fromList list

fromPredicate :: (a -> Bool) -> Predicate a
fromPredicate = Predicate

singleton :: Eq a => a -> Predicate a
singleton a = Predicate (== a)

intersection :: Predicate a -> Predicate a -> Predicate a
intersection (Predicate p1) (Predicate p2) = Predicate ((&&) <$> p1 <*> p2)

complement :: Predicate a -> Predicate a
complement (Predicate p) = Predicate (not . p)

member :: a -> Predicate a -> Bool
member a (Predicate p) = p a


-- | The 'Semigroup' instance implements the union of the sets described by 'Predicate's.
-- prop> \ a b c -> member a (fromList b <> fromList c) == member a (fromList (b <> c))
instance Semigroup (Predicate a) where
  Predicate p1 <> Predicate p2 = Predicate ((||) <$> p1 <*> p2)

instance Monoid (Predicate a) where
  mempty = Predicate (const False)
  mappend = (<>)
