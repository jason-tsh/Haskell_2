module Main where

import REPL
import Expr
import Data.List
import Control.Monad.Trans.State.Strict
import System.IO
import System.Console.Haskeline

settings :: Settings (StateT LState IO)
settings = Settings {
            complete = completeFunc,
            historyFile = Just "hist.txt",
            autoAddHistory = True
            }

commandList :: [String] -- Reserved keywords
commandList = ["input", "print", "if", "then", "else",
               "repeat", "while", "do", "for", "quit"]

completeFunc :: CompletionFunc (StateT LState IO)
completeFunc = completeWord Nothing " \t" generator

generator :: String -> StateT LState IO [Completion] -- Get list of possible words
generator str = do st <- get
                   return $ map simpleCompletion
                          $ filter (str `isPrefixOf`) (map fst3 (vars st) ++ commandList)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          flip evalStateT initLState $ runInputT settings repl 
