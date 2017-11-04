{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Parsing where

import Control.Applicative
import Data.Bifunctor (first)
import Data.List (union)
import Prelude hiding (fail)

type Symbol s = (Ord s, Show s)

class (Alternative p, Symbol s) => Parsing s p | p -> s where
  symbol :: s -> p s

newtype Empty s a = Empty { runEmpty :: Bool }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative (Empty s) where
  pure _ = Empty True

  Empty a <*> Empty b = Empty (a && b)

instance Alternative (Empty s) where
  empty = Empty False
  Empty a <|> Empty b = Empty (a || b)

instance Symbol s => Parsing s (Empty s) where
  symbol _ = Empty False

combine :: Symbol s => Empty s a -> [s] -> [s] -> [s]
combine (Empty e) s1 s2 = s1 `union` if e then s2 else []


newtype First s a = First { runFirst :: [s] }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


ferr :: First s a -> (a, String) -> First s a
ferr = const

fsymbol :: Symbol s => s -> First s a
fsymbol s = First [s]

falt :: Symbol s => First s a -> First s a -> First s a
First s1 `falt` First s2 = First (s1 `union` s2)

fpure :: a -> First s a
fpure _ = First []


newtype EmptyFirst s a = EmptyFirst { runEmptyFirst :: (Empty s a, First s a) }

instance Functor (EmptyFirst s) where
  fmap g (EmptyFirst (e, f)) = EmptyFirst (fmap g e, fmap g f)

instance Symbol s => Applicative (EmptyFirst s) where
  pure a = EmptyFirst (pure a, fpure a)

  EmptyFirst (e1, First f1) <*> EmptyFirst ~(e2, First f2) = EmptyFirst (e1 <*> e2, First (f1 `fseq` f2))
    where fseq = combine e1

instance Symbol s => Alternative (EmptyFirst s) where
  empty = EmptyFirst (empty, First [])
  EmptyFirst (e1, f1) <|> EmptyFirst (e2, f2) = EmptyFirst (e1 <|> e2, f1 `falt` f2)

instance Symbol s => Parsing s (EmptyFirst s) where
  symbol s = EmptyFirst (symbol s, fsymbol s)


type Input s = [s]
type Follow s = [s]

type DetCont s a = Input s -> Follow s -> Either String (a, Input s)

ppure :: a -> DetCont s a
ppure a = \ i _ -> Right (a, i)

psymbol :: Symbol s => s -> DetCont s s
psymbol s []      _ = Left "unexpected eof"
psymbol s (_:inp) _ = Right (s, inp)

newtype Det s a = Det { runDet :: (EmptyFirst s a, DetCont s a) }

invokeDet :: Symbol s => Det s a -> Input s -> Either String a
invokeDet (Det (_,p)) inp = case p inp [] of
  Left s        -> Left s
  Right (a, []) -> Right a
  Right _       -> Left "no rule to match at eof"

instance Functor (Det s) where
  fmap f (Det (ef, cont)) = Det (fmap f ef, fmap (fmap (first f)) . cont)

instance Symbol s => Applicative (Det s) where
  pure a = Det (pure a, ppure a)

  Det (ef1, p1) <*> ~(Det (ef2@(EmptyFirst (e2, First f2)), p2)) = Det (ef1 <*> ef2, p1 `pseq` p2)
    where p1 `pseq` p2 = \ inp follow -> do
            let comb = combine e2
            (v1, inp1) <- p1 inp (f2 `comb` follow)
            (v2, inp2) <- p2 inp1 follow
            pure (v1 v2, inp2)

instance Symbol s => Alternative (Det s) where
  empty = Det (empty, \ _ _ -> error "empty")

  Det (ef1@(EmptyFirst (Empty e1, First f1)), p1) <|> Det (ef2@(EmptyFirst (Empty e2, First f2)), p2) = Det (ef1 <|> ef2, p1 `palt` p2)
    where p1 `palt` p2 = p
            where p [] follow
                    | e1        = p1 [] follow
                    | e2        = p2 [] follow
                    | otherwise = Left "unexpected eof"
                  p inp@(s:_) follow
                    |     s `elem` f1     = p1 inp follow
                    |     s `elem` f2     = p2 inp follow
                    | e1, s `elem` follow = p1 inp follow
                    | e2, s `elem` follow = p2 inp follow
                    | otherwise           = Left ("unrecognized symbol " ++ show s)

instance Symbol s => Parsing s (Det s) where
  symbol s = Det (symbol s, psymbol s)
