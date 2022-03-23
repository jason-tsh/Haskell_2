module REPL where

import Data_type
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

data LState = LState { scope :: Int, vars :: [(Name, Value, Int)], errorFlag :: Bool }

initLState :: LState
initLState = LState 0 [] False

strVal val = Val $ StrVal val
intVal val = Val $ NumVal $ Int val

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> Int -> [(Name, Value, Int)] -> [(Name, Value, Int)]
updateVars name val scope vars = (name, val, scope) : dropVar name vars

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value, Int)] -> [(Name, Value, Int)]
dropVar name = filter (\var -> fst3 var /= name)

checkCond :: LState -> Expr -> Bool
checkCond st cond = case eval (vars st) (If cond (intVal 1) (intVal 0)) of
                      Just (NumVal (Int 1)) -> True
                      _ -> False

checkScope :: LState -> [Command] -> Bool
checkScope st [] = True
checkScope st (x:xs) = case x of
                         (Set var e) -> case eval (vars st) e of
                                             Just val -> checkScope st {vars = updateVars var val (scope st) (vars st)} xs
                                             _ -> False
                         (Print e) -> condCheck e $ checkScope st xs
                         (Cond cond x y) -> condCheck cond (blockCheck x) && condCheck cond (blockCheck y)
                         (Repeat acc cmd) -> blockCheck cmd
                         (While cond cmd) -> condCheck cond $ blockCheck cmd
                         (DoWhile cond cmd) -> condCheck cond $ blockCheck cmd
                         (For init cond after cmd) -> condCheck cond $ blockCheck (init ++ after ++ cmd)
                         (Read file) -> checkScope st xs
                         Quit -> checkScope st xs
                         where blockCheck cmd = checkScope st {scope = scope st + 1} cmd && checkScope st xs
                               condCheck cond check = case eval (vars st) cond of
                                                        Just val -> check
                                                        _ -> False

checkError :: LState -> LState -> InputT (StateT LState IO) ()
checkError st st' = if errorFlag st' then lift $ put st else lift $ put st {vars = filter (\var -> lst3 var <= scope st) (vars st')}

batch :: [Command] -> InputT (StateT LState IO) ()
batch = foldr ((>>) . process) (return ())

clear :: StateT LState IO ()
clear = do st <- get
           put st {vars = filter (\var -> lst3 var <= scope st) (vars st), errorFlag = False}
           return ()

process :: Command -> InputT (StateT LState IO) ()
process (Set var e) = do
     st <- lift get
     if errorFlag st then return () else do
     let varScope = case extract var (vars st) of
                      Just (var, val, vScope) -> vScope
                      Nothing -> scope st
     let go e = case eval (vars st) e of
                    Just val -> st {vars = updateVars var val varScope (vars st)}
                    Nothing -> st {errorFlag = True}
     let exit st = lift $ put st
     outputStrLn $ show (eval (vars st) e)
     case e of
          Val (StrVal "input") -> do inp <- getInputLine "> "
                                     case parse pExpr $ fromMaybe "" inp of
                                        [(e',"")] -> exit $ go e'
                                        _ -> do outputStrLn "Invalid input, action aborted"
                                                exit st {errorFlag = True}
          _ -> do -- st' should include the variable set to the result of evaluating e
                  Control.Monad.when (isNothing (eval (vars st) e)) $
                                     outputStrLn "Referred data not found, action aborted"
                  exit $ go e

process (Print e)
     = do st <- lift get
          if errorFlag st then return () else do
          --outputStrLn $ show (Print e)
          case eval (vars st) e of
                    Just val -> outputStrLn $ show val
                    Nothing -> do outputStrLn "No entry/ valid result found"
                                  lift $ put st {errorFlag = True}
          -- Print the result of evaluation

process (Cond cond x y)
     = do --outputStrLn $ show (Cond cond x y)
          st <- lift get
          if errorFlag st then return () else do
          case eval (vars st) cond of
            Just (NumVal (Int int)) -> if int /= 0 then batch x else batch y
            Just (Bool bool) -> if bool then batch x else batch y
            _ -> do outputStrLn "Non-deterministic condition, action aborted"
                    lift $ put st {errorFlag = True}

process (Repeat acc cmd)
     = do --outputStrLn $ show (Repeat acc cmd)
          st <- lift get
          if errorFlag st then return () else do
          lift $ put st {scope = scope st + 1}
          --outputStrLn $ show (vars st)++ show (scope st)
          if checkScope st {scope = scope st + 1} cmd
          then if acc > 0 && not (null cmd) then batch cmd >> process (Repeat (acc - 1) cmd)
                                            else do st' <- lift get
                                                    if errorFlag st'
                                                    then lift $ put st
                                                    else lift $ put st {vars = filter (\var -> lst3 var <= scope st) (vars st')}
          else do outputStrLn "**Some variables not in scope**"
                  lift $ put st {errorFlag = True}

process (While cond cmd)
     = do --outputStrLn $ show (While cond cmd)
          st <- lift get
          if errorFlag st then return () else do
          --outputStrLn $ show (vars st) ++ show (scope st)
          process (Cond cond [Repeat 1 cmd] [])
          st' <- lift get
          if checkScope st' cmd
          then Control.Monad.when (checkCond st' cond) $ process (While cond cmd)
          else do outputStrLn "**Some variables not in scope**"
                  lift $ put st {errorFlag = True}
          st' <- lift get
          checkError st st'

process (DoWhile cond cmd)
     = do --outputStrLn $ show (DoWhile cond cmd)
          process (Repeat 1 cmd) >> process (While cond cmd) -- Do then While

process (For init cond after cmd)
     = do --outputStrLn $ show (For init cond after cmd)
          st <- lift get
          if errorFlag st then return () else do
          lift $ put st {scope = scope st + 1}
          batch init
          st' <- lift get
          if checkScope st' (init ++ Cond cond [Quit] [Quit] : after ++ cmd)
          then Control.Monad.when (checkCond st' cond) $ process (While cond (cmd ++ after))
          else do outputStrLn "**Some variables not in scope**"
                  lift $ put st {errorFlag = True}
          st' <- lift get
          checkError st st'

process (Read file)
     = do case file of
            "input" -> do inp <- getInputLine "File: "
                          process (Read $ fromMaybe "" inp)
            _ -> do exist <- lift $ lift $ doesFileExist file
                    if exist then do content <- lift $ lift $ readFile file
                                     case parse pBatch content of
                                       [(list, "")] -> do st <- lift get
                                                          outputStrLn $ show list
                                                          if checkScope st list
                                                          then batch list
                                                          else outputStrLn "**Some variables not in scope**"
                                       _ -> outputStrLn "File parse error"
                    else outputStrLn "File does not exist"

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
          lift clear
          st <- lift get
          outputStrLn $ show (vars st)++ show (scope st)
          repl