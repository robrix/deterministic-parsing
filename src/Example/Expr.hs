module Example.Expr where

import Control.Applicative
import Text.Deterministic.Parser
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

data Expr = Lit Integer | Expr :+ Expr | Expr :* Expr
  deriving (Eq, Ord, Show)

lit :: Parser Char Expr
lit = Lit <$> integer

expr :: Parser Char Expr
expr = term `chainl1` ((:+) <$ char '+') <?> "expr"

term :: Parser Char Expr
term = factor `chainl1` ((:*) <$ char '*') <?> "term"

factor :: Parser Char Expr
factor = parens expr <|> lit
