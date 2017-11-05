module Data.Map.Symbolic
( Map
, lookup
) where

import Control.Applicative
import Data.Semigroup
import Prelude hiding (lookup)

newtype Map i a = Map (i -> Maybe a)

lookup :: i -> Map i a -> Maybe a
lookup i (Map m) = m i


instance Semigroup (Map i a) where
  Map p1 <> Map p2 = Map ((<|>) <$> p1 <*> p2)

instance Monoid (Map i a) where
  mempty = Map (const Nothing)
  mappend = (<>)
