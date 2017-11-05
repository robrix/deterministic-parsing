module Data.Set.Symbolic
( Set
) where

data Set a = Set (a -> Bool)
