module Data.Set.Symbolic where

data Set a = Set (a -> Bool)
