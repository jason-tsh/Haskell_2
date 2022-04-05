module REPL where

import Data_type
import REPL_misc
import Expr
import Expr_parsing
import Parsing
import Data.Maybe
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Control.Monad
import System.Console.Haskeline
import System.Exit
import System.Directory

-- It executes a list of commands in order
batch :: [Command] -> InputT (StateT LState IO) ()
batch = foldr ((>>) . process) (return ())

-- It execute a command based on the current state of the program
process :: Command -> InputT (StateT LState IO) ()
process (Set var e) = do
     st <- lift get -- get initial state
     if errorFlag st then Control.Monad.void (outputStrLn previousError) else -- halt if error flag is set
          do let varScope = case extract var (vars st) of -- determine the scope of a variable
                              Left (var, val, vScope) -> vScope -- existing ones are unchanged
                              _ -> scope st -- newly created has the same scope as the state
             let set e = case eval (vars st) e of -- evaluate the value of the variable & update the state
                           Left val -> lift $ put st {vars = updateVars var val varScope (vars st)}
                           Right msg -> abort st msg
             case e of
               Val (StrVal "input") -> do inp <- getInputLine "> " -- get user input
                                          case parse pExpr $ fromMaybe "" inp of
                                            [(e',"")] -> set e' -- set the value as the input
                                            _ -> abort st "**Invalid input, action aborted**" -- if an error occurs
               _ -> set e

process (Print e)
     = do st <- lift get -- get initial state
          if errorFlag st then Control.Monad.void (outputStrLn previousError) -- halt if error flag is set
                          else case eval (vars st) e of
                                 Left val -> outputStrLn $ show val -- Print the result of evaluation
                                 Right msg -> abort st msg

process (Cond cond x y)
     = do st <- lift get -- get initial state
          if errorFlag st then Control.Monad.void (outputStrLn previousError) -- halt if error flag is set
                          else case eval (vars st) cond of -- evaluate the Boolean expression
                                 Left (Bool bool) -> if bool then batch x else batch y
                                 _ -> abort st boolError

process (Repeat acc cmd)
     = do st <- lift get -- get initial state
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else -- halt if error flag is set 
               do lift $ put st {scope = scope st + 1} -- entering local scope
                  if checkScope st {scope = scope st + 1} cmd -- if the loop is properly formed
                  then if acc > 0 && not (null cmd) -- skip empty body
                       then batch cmd >> process (Repeat (acc - 1) cmd)
                       else updateState st
                  else abort st scopeParseError

process (While cond cmd)
     = do st <- lift get -- get initial state
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else -- halt if error flag is set
               do if checkScope st cmd -- if the loop is properly formed
                  then process (Cond cond [Repeat 1 cmd, While cond cmd] []) -- iterate once & check again
                  else abort st scopeParseError
                  updateState st

process (DoWhile cond cmd) = process (Repeat 1 cmd) >> process (While cond cmd) -- Do then While

process (For init cond after cmd)
     = do st <- lift get -- get initial state
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else -- halt if error flag is set
               do lift $ put st {scope = scope st + 1} -- entering local scope
                  batch init
                  st' <- lift get -- get initialized state
                  if checkScope st' (init ++ Cond cond [Quit] [Quit] : after ++ cmd) -- if the loop is properly formed
                  then process (Cond cond [While cond (cmd ++ after)] [])
                  else abort st scopeParseError
                  updateState st

process (Read file)
     = do st <- lift get -- get initial state
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else -- halt if error flag is set
               do case file of
                    "input" -> do inp <- getInputLine "File: " -- filename must not be 'input'
                                  process (Read $ fromMaybe "" inp)
                    _ -> do exist <- lift $ lift $ doesFileExist file
                            if exist
                            then do content <- lift $ lift $ readFile file
                                    case parse pBatch content of
                                      [(list, "")] -> if checkScope st list then batch list
                                                                            else abort st scopeParseError
                                      _ -> abort st $ "**File parse error -- " ++ file ++ "**"
                            else abort st $ "**File does not exist -- " ++ file ++ "**"

process (SetFunc name' argv cmd)
     = do st <- lift get -- get initial state
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else -- halt if error flag is set
               do let func = current st -- current function being executed
                  if checkScope st [SetFunc name' argv cmd] -- if the function is properly formed
                  then case name func of
                         "" -> do if name' `elem` map name (funcList st) -- not inside any functions
                                  then abort st $ duplicateFunc name'
                                  else lift $ put st {funcList = FuncData name' argv cmd [] [] : funcList st}
                         _ -> do if name' `elem` map name (funcList st) && uniqueFunc name' [func] -- inside a function
                                 then abort st $ duplicateFunc name'
                                 else do let root = saveFunc (FuncData name' argv cmd [func] []) [func]
                                         lift $ put st {current = func {children = FuncData name' argv cmd [func] [] : children func},
                                                        funcList = root : filter (\x -> name x /= name root) (funcList st)}
                  else abort st scopeParseError

process (Func name' argv')
     = do st <- lift get -- get initial state
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else -- halt if error flag is set
               do let func = if null $ name (current st) then iterSearch name' (funcList st) else recurSearch name' [current st]
                  case func of -- if there is an instance with matching name
                    Left x -> if length argv' == length (argv x)
                              then do lift $ put st {scope = scope st + 1, current = x} -- entering local scope
                                      batch $ zipCommand (argv x) argv' st -- setting local variables
                                      batch (body x) -- executing the body
                              else abort st "**Invalid number of arguments**"
                    Right msg -> abort st msg
                  updateState st
     where zipCommand [] [] st = [] -- non-exhaustive as parser already filtered out remaining possiblities
           zipCommand (x:xs) (y:ys) st = case eval (vars st) y of
                                           Left y' -> Set x (Val y') : zipCommand xs ys st
                                           _ -> []

process Quit = lift $ lift exitSuccess -- Unconditional termination

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
repl :: InputT (StateT LState IO) ()
repl = do inp <- getInputLine "> "
          case parse pCommand $ fromMaybe "" inp of
            [(cmd, "")] -> process cmd -- Must parse entire input
            _ -> outputStrLn "**Parse error**"
          lift clear
          repl