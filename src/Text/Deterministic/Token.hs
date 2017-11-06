module Text.Deterministic.Token where

data Token s = Token
  { tokenSymbol :: !s
  , tokenOffset :: {-# UNPACK #-} !Offset
  }

data Offset = Offset
  { offsetBytes   :: {-# UNPACK#-} !Int
  , offsetLines   :: {-# UNPACK#-} !Int
  , offsetColumns :: {-# UNPACK#-} !Int
  }
