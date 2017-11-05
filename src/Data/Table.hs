{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Table
( Table(toList)
, fromList
, singleton
) where

import Data.Function (on)
import Data.List (unionBy)
import Data.Semigroup

newtype Table i a = Table { toList :: [(i, a)] }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

fromList :: [(i, a)] -> Table i a
fromList = Table

singleton :: i -> a -> Table i a
singleton i a = Table [(i, a)]


instance Eq i => Semigroup (Table i a) where
  Table t1 <> Table t2 = Table (unionBy ((==) `on` fst) t1 t2)

instance Eq i => Monoid (Table i a) where
  mempty = Table []
  mappend = (<>)
