{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes #-}
module Text.Deterministic.Parser where

import Control.Applicative
import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Semigroup
import Text.Parser.Char
import Text.Parser.Combinators

type Symbol s = (Ord s, Show s)

data State s = State
  { stateInput :: ![s]
  , stateFollow :: ![Set.Set s]
  }

type Result s = Either (Error s)
type Success s a r = a -> State s -> r
type Failure s a r = Error s -> State s -> r
type ParserCont s a r = State s -> Success s a r -> Failure s a r -> r

newtype Table s a = Table { tableBranches :: Map.Map s a }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

data Parser s a = Parser
  { parserNull :: Maybe a
  , parserFirst :: Set.Set s
  , parserLabels :: Set.Set (Either String s)
  , parserTable :: forall r . [(s, ParserCont s a r)]
  }

data Error s = Error
  { errorExpected :: Set.Set (Either String s)
  , errorActual :: Maybe s
  }
  deriving (Eq, Ord, Show)

formatError :: Symbol s => Error s -> String
formatError (Error expected actual) = "expected (" ++ intercalate ", " (map (either id show) (Set.toList expected)) ++ ") " ++ maybe "at end" (("but got " ++) . show) actual

parse :: Symbol s => Parser s a -> [s] -> Result s a
parse (Parser e _ labels table) input = do
  (a, rest) <- choose e labels (Table (Map.fromList table)) (State input []) (curry Right) (const . Left)
  case stateInput rest of
    []  -> Right a
    c:_ -> Left (Error mempty (Just c))

instance Functor (Parser s) where
  fmap g (Parser n f l table) = Parser (fmap g n) f l (fmap (second (\ cont state yield -> cont state (yield . g))) table)

instance Symbol s => Applicative (Parser s) where
  pure a = Parser (Just a) mempty mempty mempty

  Parser n1 f1 l1 t1 <*> ~(Parser n2 f2 l2 t2) = Parser (n1 <*> n2) (combine n1 f1 f2) (combine n1 l1 l2) (t1 `tseq` t2)
    where table2 = Table (Map.fromList t2)
          t1 `tseq` t2
            = fmap (second (\ p state yield err ->
              p state { stateFollow = f2 : stateFollow state } (\ f state' ->
                choose n2 l2 table2 state' (\ a state'' ->
                  let fa = f a in fa `seq` yield fa state'') err) err)) t1
            <> case n1 of
              Just f -> fmap (second (\ q state yield err ->
                q state (\ a state' ->
                  let fa = f a in fa `seq` yield fa state') err)) t2
              _ -> mempty

combine :: Ord b => Maybe a -> Set.Set b -> Set.Set b -> Set.Set b
combine (Just _) s1 s2 = s1 <> s2
combine _        s1 _  = s1

choose :: Symbol s => Maybe a -> Set.Set (Either String s) -> Table s (ParserCont s a r) -> ParserCont s a r
choose nullible labels (Table b) = go
  where go state yield err = case stateInput state of
          []  -> maybe (err (Error labels Nothing)) yield nullible state
          c:_ -> fromMaybe (notFound c) (Map.lookup c b) state yield err
        notFound c state yield err = case nullible of
          Just a | any (c `Set.member`) (stateFollow state) -> yield a state
          _                                                 -> err (Error labels (Just c)) state

instance Symbol s => Alternative (Parser s) where
  empty = Parser Nothing mempty mempty mempty

  Parser n1 f1 l1 t1 <|> Parser n2 f2 l2 t2 = Parser (n1 <|> n2) (f1 <> f2) (l1 <> l2) (t1 <> t2)

instance Symbol s => Parsing (Parser s) where
  try = id

  p <?> label = p { parserLabels = Set.singleton (Left label) }

  unexpected _ = Parser Nothing mempty mempty mempty

  eof = Parser (Just ()) mempty mempty mempty <?> "eof"

  notFollowedBy _ = Parser (Just ()) mempty mempty mempty

instance CharParsing (Parser Char) where
  satisfy _ = empty
  anyChar = Parser Nothing (Set.fromList [minBound..maxBound]) (Set.singleton (Left "any char")) mempty
  char = symbol

symbol :: s -> Parser s s
symbol s = Parser Nothing (Set.singleton s) (Set.singleton (Right s)) [(s, \ state yield err -> case stateInput state of
  []     -> err (Error (Set.singleton (Right s)) Nothing) state
  _:rest -> yield s (state { stateInput = rest }))]