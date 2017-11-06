module Text.Deterministic.Token where

data Token s = Token
  { tokenSymbol :: !s
  , tokenDelta  :: {-# UNPACK #-} !Delta
  }

data Delta = Delta
  { deltaBytes   :: {-# UNPACK#-} !Int
  , deltaLines   :: {-# UNPACK#-} !Int
  , deltaColumns :: {-# UNPACK#-} !Int
  }
