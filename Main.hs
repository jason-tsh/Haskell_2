module Main where

import REPL
import Expr
import Data.List
import Control.Monad.Trans.State.Strict
import System.IO
import System.Console.Haskeline

commandList :: [String]
commandList = ["input", "print", "if", "then", "else",
               "repeat", "while", "do", "for", "quit"]

generator :: String -> StateT LState IO [Completion]
generator str = do st <- get
                   return $ map simpleCompletion
                          $ filter (str `isPrefixOf`) (map fst3 (vars st) ++ commandList)

settings :: Settings (StateT LState IO)
settings = Settings {
            complete = completeFunc,
            historyFile = Just "hist.txt",
            autoAddHistory = True
            }

completeFunc :: CompletionFunc (StateT LState IO)
completeFunc = completeWord Nothing " \t" generator

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          flip evalStateT initLState $ runInputT settings repl 
