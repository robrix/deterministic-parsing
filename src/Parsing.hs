{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Parsing where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Semigroup
import Text.Parser.Combinators

type Symbol s = (Ord s, Show s)

type State s = [s]
type Noskip s = [Set.Set s]

type ParserCont s a = State s -> Noskip s -> Either String (a, State s)

newtype Table s a = Table { tableBranches :: Map.Map s a }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

data Parser s a
  = Parser
    { parserNull :: Maybe a
    , parserFirst :: Set.Set s
    , parserTable :: Table s (ParserCont s a)
    }

parse :: Symbol s => Parser s a -> State s -> Either String a
parse (Parser e _ table) state = do
  (a, rest) <- choose e table state []
  case rest of
    [] -> Right a
    _ -> Left "no rule to match at end"

instance Functor (Parser s) where
  fmap g (Parser n f table) = Parser (fmap g n) f (fmap (fmap (fmap (first g)) .) table)

instance Symbol s => Applicative (Parser s) where
  pure a = Parser (Just a) mempty mempty

  Parser n1 f1 t1 <*> ~(Parser n2 f2 t2) = Parser (n1 <*> n2) (combine n1 f1 f2) (t1 `tseq` t2)
    where t1 `tseq` t2
            = fmap (\ p state noskip -> do
              (f, state') <- p state (f2 : noskip)
              (a, state'') <- choose n2 t2 state' noskip
              let fa = f a
              fa `seq` pure (fa, state'')) t1
            <> case n1 of
              Just f -> fmap (\ q state noskip -> do
                (a, state') <- q state noskip
                let fa = f a
                fa `seq` pure (fa, state')) t2
              _ -> mempty
          combine (Just _) s1 s2 = s1 <> s2
          combine _        s1 _  = s1

choose :: Symbol s => Maybe a -> Table s (ParserCont s a) -> ParserCont s a
choose (Just a) _         []     _      = Right (a, [])
choose _        _         []     _      = Left "no rule to match at end"
choose nullible (Table b) (c:cs) noskip = fromMaybe notFound (Map.lookup c b) (c:cs) noskip
  where notFound _ _
          | Just a <- nullible
          , any (c `Set.member`) noskip = Right (a, c:cs)
          | otherwise                   = Left ("expected " ++ expected ++ " but got " ++ show c)
        expected = "(" ++ intercalate ", " (map show (Map.keys b)) ++ ")"

instance Symbol s => Alternative (Parser s) where
  empty = Parser Nothing mempty mempty

  Parser n1 f1 t1 <|> Parser n2 f2 t2 = Parser (n1 <|> n2) (f1 <> f2) (t1 <> t2)

symbol :: s -> Parser s s
symbol s = Parser Nothing (Set.singleton s) (Table (Map.singleton s (\ inp _ -> case inp of
  []      -> Left "unexpected eof"
  (_:inp) -> Right (s, inp))))


data Expr = Lit Integer | Expr :+ Expr | Expr :* Expr
  deriving (Eq, Ord, Show)

lit :: Parser Char Expr
lit = Lit . fst . foldr (\ d (v, p) -> (d * p + v, p * 10)) (0, 1) <$> some (asum (map (fmap (fromIntegral . digitToInt) . symbol) ['0'..'9']))

parens :: Parser Char a -> Parser Char a
parens a = symbol '(' *> a <* symbol ')'

expr :: Parser Char Expr
expr = term `chainl1` ((:+) <$ symbol '+')

term :: Parser Char Expr
term = factor `chainl1` ((:*) <$ symbol '*')

factor :: Parser Char Expr
factor = parens expr <|> lit
