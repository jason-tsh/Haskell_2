module REPL where

import Expr
import Expr_parsing
import Parsing
import Data.Maybe
import System.Console.Haskeline
import Data.List
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Control.Monad
import System.Exit

data LState = LState { vars :: [(Name, Value)] }

initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val vars = (name, val) : filter (\var -> fst var /= name) vars

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name = filter (\var -> fst var /= name)

strVal val = Val $ StrVal val
intVal val = Val $ NumVal $ Int val

checkCond :: LState -> Expr -> Bool
checkCond st cond = case eval (vars st) (If cond (intVal 1) (intVal 0)) of
                      Just (NumVal (Int 1)) -> True
                      _ -> False

batch :: [Command] -> InputT (StateT LState IO) ()
batch = foldr ((>>) . process) (return ())

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
                  Control.Monad.when (isNothing (eval list e)) $
                                     outputStrLn "Referred data not found, action aborted"
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
          case eval (vars st) cond of
            Just (NumVal (Int int)) -> if int /= 0 then process x else process y
            _ -> outputStrLn "Non-deterministic condition, action aborted"

process (Repeat acc cmd)
     = do outputStrLn $ show (Repeat acc cmd)
          st <- lift get
          if acc > 0 && not (null cmd) then batch cmd >> process (Repeat (acc-1) cmd)
                                       else do outputStrLn "--Repeat loop exits--" --debug
                                               lift $ put st

process (While cond cmd)
     = do outputStrLn $ show (While cond cmd)
          process (Cond cond (Repeat 1 cmd) (Print $ strVal "--While loop exits--")) --debug
          st <- lift get
          Control.Monad.when (checkCond st cond) $ process (While cond cmd)

process (DoWhile cond cmd)
     = do outputStrLn $ show (DoWhile cond cmd)
          process (Repeat 1 cmd) >> process (While cond cmd) -- Do then While

process (For init cond after cmd)
     = do outputStrLn $ show (For init cond after cmd)
          batch init
          st <- lift get
          Control.Monad.when (checkCond st cond) $ process (While cond (cmd ++ after))

process Quit = lift $ lift exitSuccess

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