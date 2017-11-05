module Data.Map.Symbolic
( Map
) where

newtype Map i a = Map (i -> Maybe a)
