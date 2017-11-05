module Data.Predicate
( Predicate
, fromList
, fromPredicate
, singleton
, intersection
, member
) where

import Data.Semigroup
import qualified Data.Set as Set

newtype Predicate a = Predicate (a -> Bool)

fromList :: Ord a => [a] -> Predicate a
fromList list = Predicate (`Set.member` Set.fromList list)

fromPredicate :: (a -> Bool) -> Predicate a
fromPredicate = Predicate

singleton :: Eq a => a -> Predicate a
singleton a = Predicate (== a)

intersection :: Predicate a -> Predicate a -> Predicate a
intersection (Predicate p1) (Predicate p2) = Predicate ((&&) <$> p1 <*> p2)

member :: a -> Predicate a -> Bool
member a (Predicate p) = p a


instance Semigroup (Predicate a) where
  Predicate p1 <> Predicate p2 = Predicate ((||) <$> p1 <*> p2)

instance Monoid (Predicate a) where
  mempty = Predicate (const False)
  mappend = (<>)
