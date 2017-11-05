module Main (main) where

import System.Environment
import System.Posix.User
import Test.DocTest

main :: IO ()
main = do
  uid <- getRealUserID
  UserEntry { homeDirectory = homeDirectory } <- getUserEntryForID uid
  getArgs >>= doctest . (++
    [ "-package-db=" ++ homeDirectory ++ "/.cabal/store/ghc-8.2.1/package.db"
    , "-package-db=dist-newstyle/packagedb/ghc-8.2.1"
    , "-package-db=dist-newstyle/build/x86_64-osx/ghc-8.2.1/deterministic-parsing-0.0.0.0/package.conf.inplace"
    , "-package=parsers"
    , "src"
    ])
