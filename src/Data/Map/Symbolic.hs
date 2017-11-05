module Data.Map.Symbolic
( Map
, lookup
) where

import Prelude hiding (lookup)

newtype Map i a = Map (i -> Maybe a)

lookup :: i -> Map i a -> Maybe a
lookup i (Map m) = m i
