{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes #-}
module Parsing where

import Control.Applicative
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

type Error = String
type Result = Either Error
type Success s a r = a -> State s -> r
type Failure s a r = Error -> State s -> r
type ParserCont s a r = State s -> Noskip s -> Success s a r -> Failure s a r -> r

newtype Table s a = Table { tableBranches :: Map.Map s a }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

data Parser s a
  = Parser
    { parserNull :: Maybe a
    , parserFirst :: Set.Set s
    , parserTable :: forall r . Table s (ParserCont s a r)
    }

parse :: Symbol s => Parser s a -> State s -> Result a
parse (Parser e _ table) state = do
  (a, rest) <- choose e table state [] (curry Right) (const . Left)
  case rest of
    [] -> Right a
    _ -> Left "no rule to match at end"

instance Functor (Parser s) where
  fmap g (Parser n f table) = Parser (fmap g n) f (fmap (\ cont state noskip yield err -> cont state noskip (yield . g) err) table)

instance Symbol s => Applicative (Parser s) where
  pure a = Parser (Just a) mempty mempty

  Parser n1 f1 t1 <*> ~(Parser n2 f2 t2) = Parser (n1 <*> n2) (combine n1 f1 f2) (t1 `tseq` t2)
    where t1 `tseq` t2
            = fmap (\ p state noskip yield err ->
              p state (f2 : noskip) (\ f state' ->
                choose n2 t2 state' noskip (\ a state'' ->
                  let fa = f a in fa `seq` yield fa state'') err) err) t1
            <> case n1 of
              Just f -> fmap (\ q state noskip yield err ->
                q state noskip (\ a state' ->
                  let fa = f a in fa `seq` yield fa state') err) t2
              _ -> mempty
          combine (Just _) s1 s2 = s1 <> s2
          combine _        s1 _  = s1

choose :: Symbol s => Maybe a -> Table s (ParserCont s a r) -> ParserCont s a r
choose nullible (Table b) = go
  where go [] _ yield err
          | Just a <- nullible = yield a []
          | otherwise          = err ("expected " ++ expected ++ " at end") []
        go (c:cs) noskip yield err = fromMaybe notFound (Map.lookup c b) (c:cs) noskip yield err
          where notFound _ _ _ _
                  | Just a <- nullible
                  , any (c `Set.member`) noskip = yield a (c:cs)
                  | otherwise                   = err ("expected " ++ expected ++ " but got " ++ show c) (c:cs)
        expected = "(" ++ intercalate ", " (map show (Map.keys b)) ++ ")"

instance Symbol s => Alternative (Parser s) where
  empty = Parser Nothing mempty mempty

  Parser n1 f1 t1 <|> Parser n2 f2 t2 = Parser (n1 <|> n2) (f1 <> f2) (t1 <> t2)

symbol :: s -> Parser s s
symbol s = Parser Nothing (Set.singleton s) (Table (Map.singleton s (\ inp _ yield err -> case inp of
  []     -> err "unexpected eof" []
  _:rest -> yield s rest)))


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
