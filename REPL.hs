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

data LState = LState { scope :: Int, vars :: Tree Name Value Int, errorFlag :: Bool,
                       current :: FuncData, funcList :: [FuncData]}

data FuncData = FuncData { name :: Name, argv :: [Name], body :: [Command],
                           parent :: [FuncData], children :: [FuncData]}

initLState = LState 0 Leaf False initFunc []

initFunc = FuncData "" [] [] [] [] -- Parent attribute is a list as there can be none

-- Update the list by add/ update the value of the corresponding variable
updateVars :: Name -> Value -> Int -> Tree Name Value Int -> Tree Name Value Int
updateVars name value scope Leaf = Node Leaf name value scope Leaf
updateVars name value scope (Node lt nName nValue nScope rt)
        | nName == name = Node lt name value scope rt
        | name < nName = Node (updateVars name value scope lt) nName nValue nScope rt
        | otherwise = Node lt nName nValue nScope (updateVars name value scope rt)

deleteMax :: Tree Name Value Int -> (Name, Value, Int, Tree Name Value Int)
deleteMax Leaf = error "Empty Tree"
deleteMax (Node lt name value scope Leaf) = (name, value, scope, lt)
deleteMax (Node lt name value scope rt) = (x, y, z, Node lt name value scope rt')
    where (x,y,z, rt') = deleteMax rt

-- Update the list by removing the corresponding variable
dropVar :: Name -> Tree Name Value Int -> Tree Name Value Int
dropVar name Leaf = Leaf
dropVar name (Node lt nName nValue nScope rt)
     | name < nName = Node (dropVar name lt) nName nValue nScope rt
     | name > nName = Node lt nName nValue nScope (dropVar name rt)
     | name == nName = case (lt, rt) of
                         (Leaf, _) -> rt
                         (_, Leaf) -> lt
                         (Node _ _ _ _ _, _) -> Node lt' newName newValue newScope rt
                           where (newName, newValue, newScope, lt') = deleteMax lt

tree2List :: Tree Name Value Int -> [(Name, Value, Int)]
tree2List Leaf = []
tree2List (Node lt nName nValue nScope rt) = [(nName, nValue, nScope)] ++ tree2List lt ++ tree2List rt

-- Update the list by removing the local variables
{-Take old state and new state and go through if new variables have greater scope
    than old state they must be deleted -}
dropVar' :: LState -> LState -> Tree Name Value Int
dropVar' st st' = filter (\var -> lst3 var <= scope st) (vars st')


-- Check if there is a function instance having the same name inside the sub-tree & traverse through parent nodes till root (empty list)
uniqueFunc :: Name -> [FuncData] -> Bool
uniqueFunc name' [] = True
uniqueFunc name' (x:xs) = name' `elem` map name (x : children x) && uniqueFunc name' (parent x)

-- Check if there is a function with the same name inside the sub-tree & traverse through parent nodes till root (empty list)
recurSearch :: Name -> [FuncData] -> Maybe FuncData
recurSearch name' [] = Nothing
recurSearch name' (x:xs) = case iterSearch name' (x : children x) of
                             Just result -> Just result
                             Nothing -> recurSearch name' (parent x)

-- Check if there is a function with the same name inside the same level of sub-tree
iterSearch :: Name -> [FuncData] -> Maybe FuncData
iterSearch name' [] = Nothing
iterSearch name' (x:xs) = if name' == name x then Just x else iterSearch name' xs

-- Update the parent nodes & traverse through them till root (empty list)
saveFunc :: FuncData -> [FuncData] -> FuncData
saveFunc tar [] = tar
saveFunc tar (dest:rest) = saveFunc dest {children = tar : children dest} (parent dest)

checkCond :: LState -> Expr -> Bool
checkCond st cond = case eval (vars st) (If cond (Val $ Bool True) (Val $ Bool True)) of
                      Just (Bool bool) -> bool
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
                         (SetFunc name argv cmd) -> blockCheck (funcInit argv ++ cmd)
                         _ -> checkScope st xs -- Read, Func & Quit commands
                         where blockCheck cmd = checkScope st {scope = scope st + 1} cmd && checkScope st xs
                               condCheck cond check = case eval (vars st) cond of
                                                        Just val -> check
                                                        _ -> False
                               funcInit [] = []
                               funcInit (x:xs) = Set x (Val $ NumVal $ Int 0) : funcInit xs

checkError :: LState -> LState -> InputT (StateT LState IO) ()
checkError st st' = if errorFlag st' then lift $ put st else lift $ put st' {scope = scope st, vars = dropVar' st st', current = current st}

abort :: LState -> String -> InputT (StateT LState IO) ()
abort st msg = do outputStrLn msg
                  lift $ put st {errorFlag = True}

clear :: StateT LState IO ()
clear = do st <- get
           put st {vars = dropVar' st st, errorFlag = False}

batch :: [Command] -> InputT (StateT LState IO) ()
batch = foldr ((>>) . process) (return ())

process :: Command -> InputT (StateT LState IO) ()
process (Set var e) = do
     st <- lift get
     if errorFlag st then return () else do
     let varScope = case extract var (vars st) of
                      Just (var, val, vScope) -> vScope
                      Nothing -> scope st
     let set e = case eval (vars st) e of
                   Just val -> st {vars = updateVars var val varScope (vars st)}
                   Nothing -> st {errorFlag = True}
     let exit st = lift $ put st
     case e of
          Val (StrVal "input") -> do inp <- getInputLine "> "
                                     case parse pExpr $ fromMaybe "" inp of
                                        [(e',"")] -> exit $ set e'
                                        _ -> abort st "Invalid input, action aborted"
          _ -> do Control.Monad.when (isNothing (eval (vars st) e)) $
                                     outputStrLn "Referred data not found, action aborted"
                  exit $ set e

process (Print e)
     = do st <- lift get
          if errorFlag st then return () else do
          case eval (vars st) e of
            Just val -> outputStrLn $ show val
            Nothing -> abort st "No entry/ valid result found"
          -- Print the result of evaluation

process (Cond cond x y)
     = do st <- lift get
          if errorFlag st then return () else do
          case eval (vars st) cond of
            Just (Bool bool) -> if bool then batch x else batch y
            _ -> abort st "Non-deterministic condition, action aborted"

process (Repeat acc cmd)
     = do st <- lift get
          if errorFlag st then return () else do
          lift $ put st {scope = scope st + 1}
          if checkScope st {scope = scope st + 1} cmd
          then if acc > 0 && not (null cmd) 
               then batch cmd >> process (Repeat (acc - 1) cmd)
               else do st' <- lift get
                       if errorFlag st' then lift $ put st else lift $ put st {vars = dropVar' st st'}
          else abort st "**Scope/ parse error**"

process (While cond cmd)
     = do st <- lift get
          if errorFlag st then return () else do
          if checkScope st cmd
          then process (Cond cond [Repeat 1 cmd, While cond cmd] [])
          else abort st "**Scope/ parse error**"
          st' <- lift get
          checkError st st'

process (DoWhile cond cmd) = process (Repeat 1 cmd) >> process (While cond cmd) -- Do then While

process (For init cond after cmd)
     = do st <- lift get
          if errorFlag st then return () else do
          lift $ put st {scope = scope st + 1}
          batch init
          st' <- lift get
          if checkScope st' (init ++ Cond cond [Quit] [Quit] : after ++ cmd)
          then process (Cond cond [While cond (cmd ++ after)] [])
          else abort st "**Scope/ parse error**"
          st' <- lift get
          checkError st st'

process (Read file)
     = do st <- lift get
          if errorFlag st then return () else do
          case file of
            "input" -> do inp <- getInputLine "File: "
                          process (Read $ fromMaybe "" inp)
            _ -> do exist <- lift $ lift $ doesFileExist file
                    if exist 
                    then do content <- lift $ lift $ readFile file
                            case parse pBatch content of
                              [(list, "")] -> do st <- lift get
                                                 if checkScope st list then batch list
                                                                       else abort st "**Scope/ parse error**"
                              _ -> abort st "File parse error"
                    else abort st "File does not exist"

process (SetFunc name' argv cmd)
     = do st <- lift get
          if errorFlag st then return () else do
          let func = current st
          if checkScope st [SetFunc name' argv cmd]
          then case name func of
                 "" -> do if name' `elem` map name (funcList st)
                          then abort st "**Duplicated function name**"
                          else lift $ put st {funcList = FuncData name' argv cmd [] [] : funcList st}
                 _ -> do if name' `elem` map name (funcList st) && uniqueFunc name' [func]
                         then abort st "**Duplicated function name**"
                         else do let root = saveFunc (FuncData name' argv cmd [func] []) [func]
                                 lift $ put st {current = func {children = FuncData name' argv cmd [func] [] : children func},
                                                funcList = root : filter (\x -> name x /= name root) (funcList st)}
          else abort st "**Scope/ parse error**"

process (Func name' argv')
     = do st <- lift get
          if errorFlag st then return () else do
          let func = if null $ name (current st) then iterSearch name' (funcList st) else recurSearch name' [current st]
          case func of
            Just x -> if length argv' == length (argv x)
                      then do lift $ put st {scope = scope st + 1, current = x}
                              batch $ zipCommand (argv x) argv' st
                              batch (body x)
                      else abort st "**Invalid number of arguments**"
            Nothing -> abort st "**Function not found**"
          st' <- lift get
          checkError st st'
     where zipCommand [] [] st = []
           zipCommand (x:xs) (y:ys) st = case eval (vars st) y of
                                        Just y' -> Set x (Val y') : zipCommand xs ys st
                                        _ -> []

process Quit = lift $ lift exitSuccess

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputT (StateT LState IO) ()
repl = do inp <- getInputLine "> "
          case parse pCommand $ fromMaybe "" inp of
            [(cmd, "")] -> process cmd -- Must parse entire input
            _ -> outputStrLn "Parse error"
          lift clear
          repl