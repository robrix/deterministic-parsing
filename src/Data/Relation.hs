{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Data.Relation
( Relation
, fromList
, fromTable
, fromRelation
, fromPredicate
, singleton
, lookup
) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Table as Table
import Prelude hiding (lookup)

newtype Relation i a = Relation (i -> Maybe a)
  deriving (Functor)

fromList :: Ord i => [(i, a)] -> Relation i a
fromList [] = mempty
fromList [(i, a)] = singleton i a
fromList list = Relation (`Map.lookup` map)
  where map = Map.fromList list

fromTable :: Ord i => Table.Table i a -> Relation i a
fromTable = fromList . Table.toList

fromRelation :: (i -> Maybe a) -> Relation i a
fromRelation = Relation

fromPredicate :: (i -> Bool) -> Relation i i
fromPredicate predicate = Relation (\ i -> guard (predicate i) *> pure i)

singleton :: Eq i => i -> a -> Relation i a
singleton i a = Relation ((*> pure a) . guard . (== i))

lookup :: i -> Relation i a -> Maybe a
lookup i (Relation m) = m i


instance Semigroup (Relation i a) where
  Relation p1 <> Relation p2 = Relation ((<|>) <$> p1 <*> p2)

instance Monoid (Relation i a) where
  mempty = Relation (const Nothing)
  mappend = (<>)
