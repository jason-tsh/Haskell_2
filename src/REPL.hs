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

batch :: [Command] -> InputT (StateT LState IO) ()
batch = foldr ((>>) . process) (return ())

process :: Command -> InputT (StateT LState IO) ()
process (Set var e) = do
     st <- lift get
     if errorFlag st then Control.Monad.void (outputStrLn previousError) else
          do let varScope = case extract var (vars st) of
                              Left (var, val, vScope) -> vScope
                              _ -> scope st
             let set e = case eval (vars st) e of
                           Left val -> st {vars = updateVars var val varScope (vars st)}
                           _ -> st {errorFlag = True}
             let exit st = lift $ put st
             case e of
               Val (StrVal "input") -> do inp <- getInputLine "> "
                                          case parse pExpr $ fromMaybe "" inp of
                                            [(e',"")] -> exit $ set e'
                                            _ -> abort st "**Invalid input, action aborted**"
               _ -> do case eval (vars st) e of
                         Right msg -> outputStrLn msg
                         _ -> outputStr ""
                       exit $ set e

process (Print e)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError)
                          else case eval (vars st) e of
                                 Left val -> outputStrLn $ show val
                                 Right msg -> abort st msg
          -- Print the result of evaluation

process (Cond cond x y)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError)
                          else case eval (vars st) cond of
                                 Left (Bool bool) -> if bool then batch x else batch y
                                 _ -> abort st boolError

process (Repeat acc cmd)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else 
               do lift $ put st {scope = scope st + 1}
                  if checkScope st {scope = scope st + 1} cmd
                  then if acc > 0 && not (null cmd)
                       then batch cmd >> process (Repeat (acc - 1) cmd)
                       else updateState st
                  else abort st scopeParseError

process (While cond cmd)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else
               do if checkScope st cmd
                  then process (Cond cond [Repeat 1 cmd, While cond cmd] [])
                  else abort st scopeParseError
                  updateState st

process (DoWhile cond cmd) = process (Repeat 1 cmd) >> process (While cond cmd) -- Do then While

process (For init cond after cmd)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else
               do lift $ put st {scope = scope st + 1}
                  batch init
                  st' <- lift get
                  if checkScope st' (init ++ Cond cond [Quit] [Quit] : after ++ cmd)
                  then process (Cond cond [While cond (cmd ++ after)] [])
                  else abort st scopeParseError
                  updateState st

process (Read file)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else
               do case file of
                    "input" -> do inp <- getInputLine "File: "
                                  process (Read $ fromMaybe "" inp)
                    _ -> do exist <- lift $ lift $ doesFileExist file
                            if exist
                            then do content <- lift $ lift $ readFile file
                                    case parse pBatch content of
                                      [(list, "")] -> do st <- lift get
                                                         if checkScope st list then batch list
                                                                               else abort st scopeParseError
                                      _ -> abort st $ "**File parse error -- " ++ file ++ "**"
                            else abort st $ "**File does not exist -- " ++ file ++ "**"

process (SetFunc name' argv cmd)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else
               do let func = current st
                  if checkScope st [SetFunc name' argv cmd]
                  then case name func of
                         "" -> do if name' `elem` map name (funcList st)
                                  then abort st $ duplicateFunc name'
                                  else lift $ put st {funcList = FuncData name' argv cmd [] [] : funcList st}
                         _ -> do if name' `elem` map name (funcList st) && uniqueFunc name' [func]
                                 then abort st $ duplicateFunc name'
                                 else do let root = saveFunc (FuncData name' argv cmd [func] []) [func]
                                         lift $ put st {current = func {children = FuncData name' argv cmd [func] [] : children func},
                                                        funcList = root : filter (\x -> name x /= name root) (funcList st)}
                  else abort st scopeParseError

process (Func name' argv')
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else
               do let func = if null $ name (current st) then iterSearch name' (funcList st) else recurSearch name' [current st]
                  case func of
                    Left x -> if length argv' == length (argv x)
                              then do lift $ put st {scope = scope st + 1, current = x}
                                      batch $ zipCommand (argv x) argv' st
                                      batch (body x)
                              else abort st "**Invalid number of arguments**"
                    Right msg -> abort st msg
                  updateState st
     where zipCommand [] [] st = []
           zipCommand (x:xs) (y:ys) st = case eval (vars st) y of
                                           Left y' -> Set x (Val y') : zipCommand xs ys st
                                           _ -> []

process Quit = lift $ lift exitSuccess

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