module Main where

import Parsing
import Expr
import REPL
import System.Console.Haskeline
import System.IO
import Control.Monad.IO.Class

main :: IO ()
main = do liftIO $ hSetBuffering stdout NoBuffering
          runInputT defaultSettings $ repl initLState
          return () 
