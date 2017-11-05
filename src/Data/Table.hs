{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Table
( Table(toList)
, fromList
, singleton
) where

import Data.Semigroup

newtype Table i a = Table { toList :: [(i, a)] }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

fromList :: [(i, a)] -> Table i a
fromList = Table

singleton :: i -> a -> Table i a
singleton i a = Table [(i, a)]
