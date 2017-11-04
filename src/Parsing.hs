{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Parsing where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char
import Data.Foldable
import qualified Data.Map as Map
import Data.List (union)
import Data.Semigroup
import Text.Parser.Combinators

type Symbol s = (Ord s, Show s)

type Input s = [s]
type Noskip s = [[s]]

type ParserCont s a = Input s -> Noskip s -> Either String (a, Input s)

newtype Table s a = Table { tableBranches :: Map.Map s a }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

data Parser s a
  = Parser
    { parserNull :: Maybe a
    , parserFirst :: [s]
    , parserTable :: Table s (ParserCont s a)
    }

parse :: Symbol s => Parser s a -> Input s -> Either String a
parse (Parser e _ table) input = do
  (a, rest) <- choose e table input []
  case rest of
    [] -> Right a
    _ -> Left "no rule to match at end"

instance Functor (Parser s) where
  fmap g (Parser n f table) = Parser (fmap g n) f (fmap (fmap (fmap (first g)) .) table)

instance Symbol s => Applicative (Parser s) where
  pure a = Parser (Just a) [] (Table mempty)

  Parser n1 f1 t1 <*> ~(Parser n2 f2 t2) = Parser (n1 <*> n2) (combine n1 f1 f2) (t1 `tseq` t2)
    where t1 `tseq` t2
            = fmap (\ p input noskip -> do
              (f, input') <- p input (f2 : noskip)
              (a, input'') <- choose n2 t2 input' noskip
              pure (f a, input'')) t1
            <> case n1 of
              Just f -> fmap (\ q input noskip -> do
                (a, input') <- q input noskip
                pure (f a, input')) t2
              _ -> mempty
          combine (Just _) s1 s2 = s1 `union` s2
          combine _        s1 _  = s1

choose :: Symbol s => Maybe a -> Table s (ParserCont s a) -> ParserCont s a
choose (Just a) _ [] _ = Right (a, [])
choose _ _ [] _ = Left "no rule to match at end"
choose _ (Table b) (c:cs) follow = case Map.lookup c b of
  Just cont -> cont (c:cs) follow
  _ -> Left ("no rule to match " ++ show c)

instance Symbol s => Alternative (Parser s) where
  empty = Parser Nothing [] (Table mempty)

  Parser n1 f1 t1 <|> Parser n2 f2 t2 = Parser (n1 <|> n2) (f1 <> f2) (t1 <> t2)

symbol :: s -> Parser s s
symbol s = Parser Nothing [s] (Table (Map.singleton s (\ inp _ -> case inp of
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
