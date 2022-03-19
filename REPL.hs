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

data LState = LState { scope :: Int, vars :: [(Name, Value, Int)] }

initLState :: LState
initLState = LState 0 []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> Int -> [(Name, Value, Int)] -> [(Name, Value, Int)]
updateVars name val scope vars = (name, val, scope) : dropVar name vars

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value, Int)] -> [(Name, Value, Int)]
dropVar name = filter (\var -> fst3 var /= name)

strVal val = Val $ StrVal val
intVal val = Val $ NumVal $ Int val

checkCond :: LState -> Expr -> Bool
checkCond st cond = case eval (vars st) (If cond (intVal 1) (intVal 0)) of
                      Just (NumVal (Int 1)) -> True
                      _ -> False

checkScope :: LState -> [Command] -> Bool
checkScope st [] = True
checkScope st (x:xs) = case x of
                         (Set var e) -> case go var $ vars st of
                                          Just x -> checkScope st xs
                                          Nothing -> case eval (vars st) e of
                                                       Just val -> checkScope st {vars = updateVars var val (scope st) (vars st)} xs
                                                       Nothing -> False
                         (Print e) -> condCheck e $ checkScope st xs
                         (Cond cond x y) -> condCheck cond $ checkScope st xs
                         (Repeat acc cmd) -> blockCheck cmd
                         (While cond cmd) -> condCheck cond $ blockCheck cmd
                         (DoWhile cond cmd) -> condCheck cond $ blockCheck cmd
                         (For init cond after cmd) -> if null init then condCheck cond $ blockCheck (after ++ cmd)
                                                                   else checkScope st (init ++ For [] cond after cmd : cmd)
                                                               
                         Quit -> checkScope st xs
                         where go key [] =  Nothing--https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC-List.html
                               go key ((x,y,z):xys)
                                 | key == x  =  Just (x,y,z)
                                 | otherwise =  go key xys
                               blockCheck cmd = checkScope st {scope = scope st + 1} cmd && checkScope st xs
                               condCheck cond check = case eval (vars st) cond of
                                                        Just val -> check
                                                        Nothing -> False

batch :: [Command] -> InputT (StateT LState IO) ()
batch = foldr ((>>) . process) (return ())

process :: Command -> InputT (StateT LState IO) ()
process (Set var e) = do
     st <- lift get
     let go e = case eval (vars st) e of
                    Just val -> st {vars = updateVars var val (scope st) (vars st)}
                    Nothing -> st
     let exit st = lift $ put st
     outputStrLn $ show (eval (vars st) e)
     case e of
          Val (StrVal "input") -> do inp <- getInputLine "> "
                                     case parse pExpr $ fromMaybe "" inp of
                                        [(e',"")] -> exit $ go e'
                                        _ -> do outputStrLn "Invalid input, action aborted"
                                                exit st
          _ -> do -- st' should include the variable set to the result of evaluating e
                  Control.Monad.when (isNothing (eval (vars st) e)) $
                                     outputStrLn "Referred data not found, action aborted"
                  exit $ go e

process (Print e)
     = do st <- lift get
          outputStrLn $ show (Print e)
          case eval (vars st) e of
                    Just val -> outputStrLn $ show val
                    Nothing -> outputStrLn "No entry/ valid result found"
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
          if checkScope st cmd
          then if acc > 0 && not (null cmd) then batch cmd >> process (Repeat (acc-1) cmd)
                                            else do outputStrLn "--Repeat loop exits--" --debug
                                                    lift $ put st
          else outputStrLn "**Variable not in scope**"

process (While cond cmd)
     = do outputStrLn $ show (While cond cmd)
          process (Cond cond (Repeat 1 cmd) (Print $ strVal "--While loop exits--")) --debug
          st <- lift get
          if checkScope st cmd
          then Control.Monad.when (checkCond st cond) $ process (While cond cmd)
          else outputStrLn "**Variable not in scope**"

process (DoWhile cond cmd)
     = do outputStrLn $ show (DoWhile cond cmd)
          process (Repeat 1 cmd) >> process (While cond cmd) -- Do then While

process (For init cond after cmd)
     = do outputStrLn $ show (For init cond after cmd)
          batch init
          st <- lift get
          if checkScope st (init ++ Cond cond Quit Quit : after ++ cmd)
          then Control.Monad.when (checkCond st cond) $ process (While cond (cmd ++ after))
          else outputStrLn "**Variable not in scope**"

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