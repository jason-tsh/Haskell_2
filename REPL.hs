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

--Tree design found in https://www.youtube.com/watch?v=dlHHflOEDpk
-- Update the list by add/ update the value of the corresponding variable
updateVars :: Name -> Value -> Int -> Tree Name Value Int -> Tree Name Value Int
updateVars name value scope Leaf = Node Leaf name value scope Leaf
updateVars name value scope (Node lt nName nValue nScope rt)
     | nName == name = Node lt name value nScope rt
     | name < nName = Node (updateVars name value scope lt) nName nValue nScope rt
     | otherwise = Node lt nName nValue nScope (updateVars name value scope rt)

tree2List :: Tree Name Value Int -> [(Name, Value, Int)]
tree2List Leaf = []
tree2List (Node lt nName nValue nScope rt) = [(nName, nValue, nScope)] ++ tree2List lt ++ tree2List rt

list2Tree :: Tree Name Value Int -> [(Name, Value, Int)] -> Tree Name Value Int
list2Tree = foldl (\ tree x -> updateVars (fst3 x) (snd3 x) (lst3 x) tree)

-- Update the list by removing the local variables
-- Take the old new state and build a new tree without the local variables by comparing their scopes
dropVar :: LState -> LState -> Tree Name Value Int
dropVar st st' = list2Tree Leaf (filter (\var -> lst3 var <= scope st) (tree2List (vars st')))


-- Check if there is a function instance having the same name inside the sub-tree & traverse through parent nodes till root (empty list)
uniqueFunc :: Name -> [FuncData] -> Bool
uniqueFunc name' [] = True
uniqueFunc name' (x:xs) = name' `elem` map name (x : children x) && uniqueFunc name' (parent x)

-- Check if there is a function with the same name inside the sub-tree & traverse through parent nodes till root (empty list)
recurSearch :: Name -> [FuncData] -> Either FuncData String
recurSearch name' [] = Right $ emptyFunction name'
recurSearch name' (x:xs) = case iterSearch name' (x : children x) of
                             Left result -> Left result
                             Right _ -> recurSearch name' (parent x)

-- Check if there is a function with the same name inside the same level of sub-tree
iterSearch :: Name -> [FuncData] -> Either FuncData String
iterSearch name' [] = Right $ emptyFunction name'
iterSearch name' (x:xs) = if name' == name x then Left x else iterSearch name' xs

-- Update the parent nodes & traverse through them till root (empty list)
saveFunc :: FuncData -> [FuncData] -> FuncData
saveFunc tar [] = tar
saveFunc tar (dest:rest) = saveFunc dest {children = tar : children dest} (parent dest)

checkCond :: LState -> Expr -> Bool
checkCond st cond = case eval (vars st) (If cond (Val $ Bool True) (Val $ Bool True)) of
                      Left (Bool bool) -> bool
                      _ -> False

checkScope :: LState -> [Command] -> Bool
checkScope st [] = True
checkScope st (x:xs) = 
     case x of
       (Set var e) -> case eval (vars st) e of
                        Left (StrVal "input") -> checkScope st {vars = updateVars var (NumVal (Int 1)) (scope st) (vars st)} xs
                        Left val -> checkScope st {vars = updateVars var val (scope st) (vars st)} xs
                        _ -> False
       (Print e) -> condCheck e $ checkScope st xs
       (Cond cond x y) -> condCheck cond (blockCheck st x) && condCheck cond (blockCheck st y)
       (Repeat acc cmd) -> blockCheck st cmd
       (While cond cmd) -> condCheck cond $ blockCheck st cmd
       (DoWhile cond cmd) -> condCheck cond $ blockCheck st cmd
       (For init cond after cmd) -> condCheck cond $ blockCheck st (init ++ after ++ cmd)
       (SetFunc name' argv cmd) ->
         do let func = current st
            case name func of
              "" -> notElem name' (map name (funcList st)) && blockCheck st {funcList = FuncData name' argv cmd [] [] : funcList st} (funcInit argv ++ cmd)
              _ -> not (name' `elem` map name (funcList st) && uniqueFunc name' [func]) &&
                       (do let root = saveFunc (FuncData name' argv cmd [func] []) [func]
                           blockCheck st {current = func {children = FuncData name' argv cmd [func] [] : children func},
                                          funcList = root : filter (\x -> name x /= name root) (funcList st)} (funcInit argv ++ cmd))
       (Func name' argv') -> do let func = if null $ name (current st) then iterSearch name' (funcList st) else recurSearch name' [current st]
                                case func of
                                  Left x -> (length argv' == length (argv x)) && checkScope st xs
                                  Right msg -> False
       (Read file) -> checkScope st xs
       Quit -> True
     where blockCheck st cmd = checkScope st {scope = scope st + 1} cmd && checkScope st xs
           condCheck cond check = case eval (vars st) cond of
                                    Left val -> check
                                    _ -> False
           funcInit [] = []
           funcInit (x:xs) = Set x (Val $ NumVal $ Int 0) : funcInit xs

updateState :: LState -> LState -> InputT (StateT LState IO) ()
updateState st st' = if errorFlag st' then lift $ put st else lift $ put st' {scope = scope st, vars = dropVar st st', current = current st}

abort :: LState -> String -> InputT (StateT LState IO) ()
abort st msg = do outputStrLn msg
                  lift $ put st {errorFlag = True}

clear :: StateT LState IO ()
clear = do st <- get
           put st {vars = dropVar st st, errorFlag = False}

batch :: [Command] -> InputT (StateT LState IO) ()
batch = foldr ((>>) . process) (return ())

process :: Command -> InputT (StateT LState IO) ()
process (Set var e) = do
     st <- lift get
     if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
     let varScope = case extract var (vars st) of
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
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
          case eval (vars st) e of
            Left val -> outputStrLn $ show val
            Right msg -> abort st msg
          -- Print the result of evaluation

process (Cond cond x y)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
          case eval (vars st) cond of
            Left (Bool bool) -> if bool then batch x else batch y
            _ -> abort st boolError

process (Repeat acc cmd)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
          lift $ put st {scope = scope st + 1}
          if checkScope st {scope = scope st + 1} cmd
          then if acc > 0 && not (null cmd)
               then batch cmd >> process (Repeat (acc - 1) cmd)
               else do st' <- lift get
                       if errorFlag st' then lift $ put st else lift $ put st {vars = dropVar st st'}
          else abort st scopeParseError

process (While cond cmd)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
          if checkScope st cmd
          then process (Cond cond [Repeat 1 cmd, While cond cmd] [])
          else abort st scopeParseError
          st' <- lift get
          updateState st st'

process (DoWhile cond cmd) = process (Repeat 1 cmd) >> process (While cond cmd) -- Do then While

process (For init cond after cmd)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
          lift $ put st {scope = scope st + 1}
          batch init
          st' <- lift get
          if checkScope st' (init ++ Cond cond [Quit] [Quit] : after ++ cmd)
          then process (Cond cond [While cond (cmd ++ after)] [])
          else abort st scopeParseError
          st' <- lift get
          updateState st st'

process (Read file)
     = do st <- lift get
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
          case file of
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
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
          let func = current st
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
          if errorFlag st then Control.Monad.void (outputStrLn previousError) else do
          let func = if null $ name (current st) then iterSearch name' (funcList st) else recurSearch name' [current st]
          case func of
            Left x -> if length argv' == length (argv x)
                      then do lift $ put st {scope = scope st + 1, current = x}
                              batch $ zipCommand (argv x) argv' st
                              batch (body x)
                      else abort st "**Invalid number of arguments**"
            Right msg -> abort st msg
          st' <- lift get
          updateState st st'
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