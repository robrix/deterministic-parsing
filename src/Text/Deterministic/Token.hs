module Text.Deterministic.Token where

import Data.Semigroup

data Token s = Token
  { tokenSymbol :: !s
  , tokenOffset :: {-# UNPACK #-} !Offset
  }

data Offset = Offset
  { offsetBytes   :: {-# UNPACK#-} !Int
  , offsetLines   :: {-# UNPACK#-} !Int
  , offsetColumns :: {-# UNPACK#-} !Int
  }

instance Semigroup Offset where
  Offset b1 l1 _ <> Offset b2 l2 c2 = Offset (b1 + b2) (l1 + l2) c2

instance Monoid Offset where
  mempty = Offset 0 0 0
  mappend = (<>)


data Interval = Interval
  { intervalStart :: {-# UNPACK #-} !Offset
  , intervalEnd   :: {-# UNPACK #-} !Offset
  }


class (Ord s, Show s) => Symbol s where
  offsetFrom :: Offset -> s -> Offset

instance Symbol Char where
  offsetFrom (Offset bytes lines _)       '\n' = Offset (succ bytes) (succ lines) 0
  offsetFrom (Offset bytes lines columns) _    = Offset (succ bytes)       lines (succ columns)
