module Main where

import Data_type
import REPL
import REPL_misc
import Expr
import Data.List
import Control.Monad.Trans.State.Strict
import System.Console.Haskeline

settings :: Settings (StateT LState IO)
settings = Settings {
            complete = completeFunc,
            historyFile = Just "./src/hist.txt", -- history file
            autoAddHistory = True -- automatically write history to the history file
            }

commandList :: [String] -- keywords (some are not included, mainly single character ones)
commandList = ["input", "print", "if", "then", "else",
               "repeat", "while", "do", "for", "read",
               "void", "quit", "abs", "mod", "&&", "||",
               "!(", "==", "++", "toNum", "toStr"]

completeFunc :: CompletionFunc (StateT LState IO)
completeFunc = completeWord Nothing " \t" generator

generator :: String -> StateT LState IO [Completion] -- Get list of possible words
generator str = do st <- get
                   return $ map simpleCompletion
                          $ filter (str `isPrefixOf`) (map fst3 (tree2List (vars st)) -- list of variables
                                                    ++ map name (funcList  st) -- list of functions
                                                    ++ commandList) -- keywords

main :: IO ()
main = flip evalStateT initLState $ runInputT settings repl
