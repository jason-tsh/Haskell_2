module REPL where

import Expr
import Parsing
import Data.Maybe
import System.Console.Haskeline
import Data.List (isPrefixOf)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import System.Exit (exitSuccess)

data LState = LState { vars :: [(Name, Value)], forLoop :: Bool }

initLState :: LState
initLState = LState [] False

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val vars = (name, val) : filter (\var -> fst var /= name) vars

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name = filter (\var -> fst var /= name)

process :: Command -> InputT (StateT LState IO) ()
process (Set var e) = do
     st <- lift get
     let list = vars st
     let go e = case eval list e of
                    Just val -> st {vars = updateVars var val list}
                    Nothing -> st
     let exit st = lift $ put st
     outputStrLn $ show (eval list e)
     case e of
          Val (StrVal "input") -> do inp <- getInputLine "> "
                                     case parse pExpr $ fromMaybe "" inp of
                                        [(e',"")] -> exit $ go e'
                                        _ -> do outputStrLn "Invalid input, action aborted"
                                                exit st
          _ -> do -- st' should include the variable set to the result of evaluating e
                  if isNothing (eval list e) then outputStrLn "Referred data not found, action aborted"
                                             else outputStr ""
                  exit $ go e
process (Print e)
     = do st <- lift get
          outputStrLn $ show (Print e)
          case eval (vars st) e of
                    Just val -> outputStrLn $ show val
                    Nothing -> outputStrLn "No entry found"
          -- Print the result of evaluation
process (Cond cond x y)
     = do outputStrLn $ show (Cond cond x y)
          st <- lift get
          let list = vars st
          case eval list cond of
            Just (NumVal (Int int)) -> do if int /= 0 then process x
                                                      else process y
            _ -> do outputStrLn "Non-deterministic condition, action aborted"
process (Repeat acc cmd)
     = do outputStrLn $ show (Repeat acc cmd)
          st <- lift get
          if acc > 0 && not (null cmd) then do go cmd
                                               process (Repeat (acc-1) cmd)
          else do outputStrLn "--Repeat loop exits--" --debug
                  lift $ put st
          where go (x:xs)  = do process x
                                go xs
                go [] = return ()
process (While cond cmd)
     = do outputStrLn $ show (While cond cmd)
          let str val = Val $ StrVal val
          let int val = Val $ NumVal $ Int val
          let bool st = case eval (vars st) (If cond (int 1) (int 0)) of
                             Just (NumVal (Int 1)) -> True
                             _ -> False
          process (Cond cond (Repeat 1 cmd) (Print $ str "--While loop exits--")) --debug
          st <- lift get
          if bool st then process (While cond cmd) else return ()

process (DoWhile cond cmd)
     = do outputStrLn $ show (DoWhile cond cmd)
          process (Repeat 1 cmd) -- Do part
          process (While cond cmd) -- While part
process (For init cond after cmd)
     = do outputStrLn $ show (For init cond after cmd)
          st <- lift get
          let int val = Val $ NumVal $ Int val
          let go (x:xs)  = do process x
                              go xs
              go [] = lift $ put st {forLoop = True}
          let bool st = case eval (vars st) (If cond (int 1) (int 0)) of
                             Just (NumVal (Int 1)) -> True
                             _ -> False
          if forLoop st then go [] else go init
          st <- lift get
          if bool st then do process (Repeat 1 cmd) 
                             go after
                             process (For init cond after cmd)
                     else lift $ put st {forLoop = False}
process Quit = lift $ lift exitSuccess

commandList = ["input", "print", "if", "then", "else",
               "repeat", "while", "do", "for", "quit"]

generator :: String -> StateT LState IO [Completion]
generator str = do st <- get
                   return $ map simpleCompletion $ filter (str `isPrefixOf`)
                                                          (map fst (vars st) ++ commandList)

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputT (StateT LState IO) ()
repl = do inp <- getInputLine "> "
          case parse pCommand $ fromMaybe "" inp of
               [(cmd, "")] -> process cmd -- Must parse entire input
               _ -> do outputStrLn "Parse error"
          repl
