module Main where

import REPL
import System.Console.Haskeline
import System.IO
import Control.Monad.Trans.State.Strict
import Data.List

commandList :: [String]
commandList = ["input", "print", "if", "then", "else",
               "repeat", "while", "do", "for", "quit"]

generator :: String -> StateT LState IO [Completion]
generator str = do st <- get
                   return $ map simpleCompletion
                          $ filter (str `isPrefixOf`) (map fst (vars st) ++ commandList)

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
