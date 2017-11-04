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

data Table s a
  = Table
    { tableBranches :: Map.Map s a
    , tableDefault :: Maybe a
    }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Symbol s => Semigroup (Table s a) where
  Table b1 d1 <> Table b2 d2 = Table (b1 <> b2) (d1 <|> d2)

instance Symbol s => Monoid (Table s a) where
  mempty = Table mempty Nothing
  mappend = (<>)

data Parser s a
  = Parser
    { parserNullible :: Bool
    , parserFirst :: [s]
    , parserTable :: Table s (ParserCont s a)
    }

parse :: Symbol s => Parser s a -> Input s -> Either String a
parse (Parser _ _ table) input = do
  (a, rest) <- choose table input []
  case rest of
    [] -> Right a
    _ -> Left "no rule to match at end"

instance Functor (Parser s) where
  fmap g (Parser n f table) = Parser n f (fmap (fmap (fmap (first g)) .) table)

instance Symbol s => Applicative (Parser s) where
  pure a = Parser True [] (Table mempty (Just (\ i _ -> Right (a, i))))

  Parser n1 f1 t1 <*> ~(Parser n2 f2 t2) = Parser (n1 && n2) (combine n1 f1 f2) (t1 `tseq` t2)
    where t1 `tseq` t2
            = fmap (\ p input noskip -> do
              (f, input') <- p input (f2 : noskip)
              (a, input'') <- choose t2 input' noskip
              pure (f a, input'')) t1
            <> case tableDefault t1 of
              Just f -> fmap (\ q input noskip -> do
                (f', input') <- f input noskip
                (a, input'') <- q input' noskip
                pure (f' a, input'')) t2
              _ -> mempty
          combine e s1 s2 = s1 `union` if e then s2 else []

choose :: Symbol s => Table s (ParserCont s a) -> ParserCont s a
choose (Table _ (Just a)) [] follow = a [] follow
choose (Table _ Nothing) [] _ = Left "no rule to match at end"
choose (Table b _) (c:cs) follow = case Map.lookup c b of
  Just cont -> cont (c:cs) follow
  _ -> Left ("no rule to match " ++ show c)

instance Symbol s => Alternative (Parser s) where
  empty = Parser True [] (Table mempty Nothing)

  Parser n1 f1 t1 <|> Parser n2 f2 t2 = Parser (n1 || n2) (f1 <> f2) (t1 <> t2)

symbol :: s -> Parser s s
symbol s = Parser False [s] (Table (Map.singleton s (\ inp _ -> case inp of
  []      -> Left "unexpected eof"
  (_:inp) -> Right (s, inp))) Nothing)


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
