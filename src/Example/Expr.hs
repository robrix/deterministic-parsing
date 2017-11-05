module Example.Expr where

import Control.Applicative
import Data.Char
import Data.Foldable
import Text.Deterministic.Parser
import Text.Parser.Char
import Text.Parser.Combinators

data Expr = Lit Integer | Expr :+ Expr | Expr :* Expr
  deriving (Eq, Ord, Show)

lit :: Parser Char Expr
lit = Lit . fst . foldr (\ d (v, p) -> (d * p + v, p * 10)) (0, 1) <$> some (asum (map (fmap (fromIntegral . digitToInt) . char) ['0'..'9'])) <?> "integer"

parens :: Parser Char a -> Parser Char a
parens a = char '(' *> a <* char ')' <?> "parens"

expr :: Parser Char Expr
expr = term `chainl1` ((:+) <$ char '+') <?> "expr"

term :: Parser Char Expr
term = factor `chainl1` ((:*) <$ char '*') <?> "term"

factor :: Parser Char Expr
factor = parens expr <|> lit
