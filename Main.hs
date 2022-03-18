module Main where

import Parsing
import Expr
import REPL
import System.Console.Haskeline
import System.IO
import Control.Monad.IO.Class

settings :: Settings IO
settings = defaultSettings {
                            autoAddHistory = True,
                            historyFile = Just "hist.txt"
                           }

main :: IO ()
main = do liftIO $ hSetBuffering stdout NoBuffering
          runInputT settings $ repl initLState
          return () 
