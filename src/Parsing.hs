{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Parsing where

import Control.Applicative
import Data.Bifunctor (first)
import Data.List (union)
import Data.Semigroup
import Prelude hiding (fail)

type Symbol s = (Ord s, Show s)

class (Alternative p, Symbol s) => Parsing s p | p -> s where
  symbol :: s -> p s

combine :: Symbol s => Bool -> [s] -> [s] -> [s]
combine e s1 s2 = s1 `union` if e then s2 else []


type Input s = [s]
type Follow s = [s]

type DetCont s a = Input s -> Follow s -> Either String (a, Input s)

ppure :: a -> DetCont s a
ppure a i _ = Right (a, i)

psymbol :: Symbol s => s -> DetCont s s
psymbol s []      _ = Left "unexpected eof"
psymbol s (_:inp) _ = Right (s, inp)

data Det s a = Det { detNullible :: Bool, detFirst :: [s], runDet :: DetCont s a }

invokeDet :: Symbol s => Det s a -> Input s -> Either String a
invokeDet (Det _ _ p) inp = case p inp [] of
  Left s        -> Left s
  Right (a, []) -> Right a
  Right _       -> Left "no rule to match at eof"

instance Functor (Det s) where
  fmap g (Det n f cont) = Det n f (fmap (fmap (first g)) . cont)

instance Symbol s => Applicative (Det s) where
  pure = Det True [] . ppure

  Det n1 f1 p1 <*> ~(Det n2 f2 p2) = Det (n1 && n2) (combine n1 f1 f2) (p1 `pseq` p2)
    where p1 `pseq` p2 = \ inp follow -> do
            let comb = combine n2
            (v1, inp1) <- p1 inp (f2 `comb` follow)
            (v2, inp2) <- p2 inp1 follow
            pure (v1 v2, inp2)

instance Symbol s => Alternative (Det s) where
  empty = Det True [] (\ _ _ -> Left "empty")

  Det n1 f1 p1 <|> Det n2 f2 p2 = Det (n1 || n2) (f1 <> f2) (p1 `palt` p2)
    where p1 `palt` p2 = p
            where p [] follow
                    | n1        = p1 [] follow
                    | n2        = p2 [] follow
                    | otherwise = Left "unexpected eof"
                  p inp@(s:_) follow
                    |     s `elem` f1     = p1 inp follow
                    |     s `elem` f2     = p2 inp follow
                    | n1, s `elem` follow = p1 inp follow
                    | n2, s `elem` follow = p2 inp follow
                    | otherwise           = Left ("unrecognized symbol " ++ show s)

instance Symbol s => Parsing s (Det s) where
  symbol s = Det False [s] (psymbol s)
