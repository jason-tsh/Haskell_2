module Main where

import Parsing
import Expr
import REPL
import System.IO

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          repl initLState
