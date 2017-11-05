{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes #-}
module Text.Deterministic.Parser where

import Control.Applicative
import Control.Monad (guard)
import qualified Data.Predicate as Predicate
import qualified Data.Relation as Relation
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Semigroup
import qualified Data.Table as Table
import Text.Parser.Char
import Text.Parser.Combinators

type Symbol s = (Ord s, Show s)

data State s = State
  { stateIndex :: {-# UNPACK #-} !Int
  , stateInput :: ![s]
  , stateFollow :: ![Predicate.Predicate s]
  }

type Result s = Either (Error s)
type Success s a r = a -> State s -> r
type Failure s a r = Error s -> State s -> r
type ParserCont s a r = State s -> Success s a r -> Failure s a r -> r

data Parser s a = Parser
  { parserNull :: Maybe a
  , parserFirst :: Predicate.Predicate s
  , parserLabels :: Set.Set (Either String s)
  , parserTable :: forall r . ParserTable s (ParserCont s a r)
  }

data ParserTable s a
  = Table (Table.Table s a)
  | Relation (Relation.Relation s a)
  deriving (Functor)

toRelation :: Symbol s => ParserTable s a -> Relation.Relation s a
toRelation (Table t) = Relation.fromTable t
toRelation (Relation r) = r

data Error s = Error
  { errorExpected :: Set.Set (Either String s)
  , errorActual :: Maybe (Either String s)
  }
  deriving (Eq, Ord, Show)

formatError :: Symbol s => Error s -> String
formatError (Error expected actual) = "expected (" ++ intercalate ", " (map (either id show) (Set.toList expected)) ++ ") " ++ maybe "at end" (("but got " ++) . either id show) actual

parse :: Symbol s => Parser s a -> [s] -> Result s a
parse (Parser e _ labels table) input = do
  (a, rest) <- choose e labels (toRelation table) (State 0 input []) (curry Right) (const . Left)
  case stateInput rest of
    []  -> Right a
    c:_ -> Left (Error mempty (Just (Right c)))

instance Functor (Parser s) where
  fmap g (Parser n f l table) = Parser (fmap g n) f l (fmap (\ cont state yield -> cont state (yield . g)) table)

instance Symbol s => Applicative (Parser s) where
  pure a = Parser (Just a) mempty mempty mempty

  Parser n1 f1 l1 t1 <*> ~(Parser n2 f2 l2 t2) = Parser (n1 <*> n2) (combine n1 f1 f2) (combine n1 l1 l2) (t1 `tseq` t2)
    where table2 = toRelation t2
          t1 `tseq` t2
            = fmap (\ p state yield err ->
              p state { stateFollow = f2 : stateFollow state } (\ f state' ->
                choose n2 l2 table2 state' (\ a state'' ->
                  let fa = f a in fa `seq` yield fa state'') err) err) t1
            <> case n1 of
              Just f -> fmap (\ q state yield err ->
                q state (\ a state' ->
                  let fa = f a in fa `seq` yield fa state') err) t2
              _ -> mempty

combine :: Semigroup b => Maybe a -> b -> b -> b
combine (Just _) s1 s2 = s1 <> s2
combine _        s1 _  = s1

choose :: Symbol s => Maybe a -> Set.Set (Either String s) -> Relation.Relation s (ParserCont s a r) -> ParserCont s a r
choose nullible labels b = go
  where go state yield err = case stateInput state of
          []  -> maybe (err (Error labels Nothing)) yield nullible state
          c:_ -> fromMaybe (notFound c) (Relation.lookup c b) state yield err
        notFound c state yield err = case nullible of
          Just a | any (c `Predicate.member`) (stateFollow state) -> yield a state
          _                                                       -> err (Error labels (Just (Right c))) state

instance Symbol s => Alternative (Parser s) where
  empty = Parser Nothing mempty mempty mempty

  Parser n1 f1 l1 t1 <|> Parser n2 f2 l2 t2 = Parser (n1 <|> n2) (f1 <> f2) (l1 <> l2) (t1 <> t2)

instance Symbol s => Parsing (Parser s) where
  try = id

  p <?> label = p { parserLabels = Set.singleton (Left label) }

  unexpected s = Parser Nothing (Predicate.complement mempty) (Set.singleton (Left ("not " ++ s))) (Relation (Relation.fromRelation (\ _ -> Just (\ state _ err -> err (Error (Set.singleton (Left ("not " ++ s))) (Just (Left s))) state))))

  eof = Parser (Just ()) mempty mempty mempty <?> "eof"

  notFollowedBy a = Parser (Just ()) (Predicate.complement (parserFirst a)) (Set.map (Left . ("not " ++) . either id show) (parserLabels a)) mempty

instance CharParsing (Parser Char) where
  satisfy predicate = Parser Nothing (Predicate.fromPredicate predicate) mempty (Relation (Relation.fromRelation (\ s -> guard (predicate s) *> pure (\ state yield _ -> yield s state))))

  notChar c = Parser Nothing (Predicate.fromPredicate (/= c)) (Set.singleton (Left ("not " ++ show c))) (Relation (Relation.fromRelation (\ s -> guard (s /= c) *> pure (\ state yield _ -> yield s (advanceState state)))))

  anyChar = Parser Nothing (Predicate.fromPredicate (const True)) (Set.singleton (Left "any char")) (Relation (Relation.fromRelation (\ s -> pure (\ state yield _ -> yield s (advanceState state)))))

  char = symbol

symbol :: Symbol s => s -> Parser s s
symbol s = Parser Nothing (Predicate.singleton s) (Set.singleton (Right s)) (Table (Table.singleton s (\ state yield _ -> yield s (advanceState state))))

advanceState :: State s -> State s
advanceState state = state { stateIndex = succ (stateIndex state), stateInput = drop 1 (stateInput state) }

instance Symbol s => Semigroup (ParserTable s a) where
  Table t1 <> Table t2 = Table (t1 <> t2)
  Relation r1 <> Relation r2 = Relation (r1 <> r2)
  Table t1 <> Relation r2 = Relation (Relation.fromTable t1 <> r2)
  Relation r1 <> Table t2 = Relation (r1 <> Relation.fromTable t2)

instance Symbol s => Monoid (ParserTable s a) where
  mempty = Table mempty
  mappend = (<>)
