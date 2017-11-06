module Text.Deterministic.Token where

data Delta = Delta
  { deltaBytes   :: {-# UNPACK#-} !Int
  , deltaLines   :: {-# UNPACK#-} !Int
  , deltaColumns :: {-# UNPACK#-} !Int
  }
