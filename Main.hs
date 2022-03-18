module Main where

import Parsing
import Expr
import REPL
import System.Console.Haskeline
import System.IO
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.State.Strict

settings :: Settings (StateT LState IO)
settings = Settings {
            complete = completeFunc,
            historyFile = Just "hist.txt",
            autoAddHistory = True
            }

completeFunc :: CompletionFunc (StateT LState IO)
completeFunc = completeWord Nothing " \t" generator

main :: IO ()
main = do liftIO $ hSetBuffering stdout NoBuffering
          flip evalStateT initLState $ runInputT settings $ repl initLState 
          return () 
