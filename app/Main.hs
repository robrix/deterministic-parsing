module Main where

import Control.Monad
import Data.Bifoldable
import Data.Foldable
import Example.Expr
import System.Environment
import Text.Deterministic.Parser

main :: IO ()
main = do
  files <- getArgs
  traverse_ (bitraverse_ (putStrLn . formatError) print . parse expr <=< readFile) files
