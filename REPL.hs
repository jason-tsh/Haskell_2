module REPL where

import Expr
import Parsing
import Data.Maybe
import System.Console.Haskeline
import Data.List (isPrefixOf)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

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

process :: LState -> Command -> InputT (StateT LState IO) LState
process st (Set var e) = do
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
     where list = vars st
           go e = case eval list e of
                    Just val -> st {vars = updateVars var val list}
                    Nothing -> st
           exit st = do lift $ put st
                        return st
process st (Print e)
     = do outputStrLn $ show (Print e)
          case eval (vars st) e of
                    Just val -> outputStrLn $ show val
                    Nothing -> outputStrLn "No entry found"
          -- Print the result of evaluation
          return st
process st (Cond cond x y)
     = do outputStrLn $ show (Cond cond x y)
          case eval list cond of
            Just (NumVal (Int int)) -> do if int /= 0 then process st x
                                                      else process st y
            _ -> do outputStrLn "Non-deterministic condition, action aborted"
                    return st
          where list = vars st
process st (Repeat acc cmd)
     = do outputStrLn $ show (Repeat acc cmd)
          if acc > 0 && not (null cmd) then do st' <- go st cmd
                                               process st' (Repeat (acc-1) cmd)
          else do outputStrLn "--Repeat loop exits--" --debug
                  return st
          where go st (x:xs)  = do st' <- process st x
                                   go st' xs
                go st [] = return st
process st (While cond cmd)
     = do outputStrLn $ show (While cond cmd)
          st' <- process st (Cond cond (Repeat 1 cmd) (Print $ str "--While loop exits--")) --debug
          if bool then process st' (While cond cmd) else return st'
          where bool = case eval list (If cond (int 1) (int 0)) of
                         Just (NumVal (Int 1)) -> True
                         _ -> False
                list = vars st
                str val = Val $ StrVal val
                int val = Val $ NumVal $ Int val
process st (DoWhile cond cmd)
     = do outputStrLn $ show (DoWhile cond cmd)
          st' <- process st (Repeat 1 cmd) -- Do part
          process st' (While cond cmd) -- While part
process st (For init cond after cmd)
     = do outputStrLn $ show (For init cond after cmd)
          st' <- if forLoop st then return st else do go st init
          st'' <- if bool st' then process st' (Repeat 1 cmd) else return st'
          st' <- if bool st'' then go st'' after else return st''
          if bool st' then process st' (For init cond after cmd) else return st' {forLoop = False}
          where go st (x:xs)  = do st' <- process st x
                                   go st' xs
                go st [] = return st {forLoop = True}
                bool st = case eval (vars st) (If cond (int 1) (int 0)) of
                         Just (NumVal (Int 1)) -> True
                         _ -> False
                int val = Val $ NumVal $ Int val
process st Quit = return st

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

repl :: LState -> InputT (StateT LState IO) LState
repl st = do inp <- getInputLine "> "
             case parse pCommand $ fromMaybe "" inp of
                    [(cmd, "")] -> -- Must parse entire input
                                   do st' <- process st cmd
                                      repl st'
                    _ -> do outputStrLn "Parse error"
                            repl st
