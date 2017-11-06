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

data Interval = Interval
  { intervalStart :: {-# UNPACK #-} !Offset
  , intervalEnd   :: {-# UNPACK #-} !Offset
  }

instance Semigroup Offset where
  Offset b1 l1 _ <> Offset b2 l2 c2 = Offset (b1 + b2) (l1 + l2) c2
