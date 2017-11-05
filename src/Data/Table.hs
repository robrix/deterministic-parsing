module Data.Table
( Table(toList)
, fromList
) where

newtype Table i a = Table { toList :: [(i, a)] }

fromList :: [(i, a)] -> Table i a
fromList = Table
