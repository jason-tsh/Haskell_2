module Main where

import Parsing
import Expr
import REPL
import System.Console.Haskeline
import System.IO
import Control.Monad.IO.Class

settings :: Settings IO
settings = Settings {
            complete = completeWord Nothing " \t" $ return . search,
            historyFile = Just "hist.txt",
            autoAddHistory = True
            }

main :: IO ()
main = do liftIO $ hSetBuffering stdout NoBuffering
          runInputT settings $ repl initLState
          return () 
