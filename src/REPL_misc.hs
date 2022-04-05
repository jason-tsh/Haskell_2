module REPL_misc where

import Data_type
import Expr
import Control.Monad.Trans.State.Strict
import System.Console.Haskeline
import Control.Monad.Trans.Class

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

updateState :: LState -> InputT (StateT LState IO) ()
updateState st = do st' <- lift get
                    if errorFlag st' then lift $ put st
                                     else lift $ put st' {scope = scope st, vars = dropVar st st', current = current st}

abort :: LState -> String -> InputT (StateT LState IO) ()
abort st msg = do outputStrLn msg
                  lift $ put st {errorFlag = True}

clear :: StateT LState IO ()
clear = do st <- get
           put st {vars = dropVar st st, errorFlag = False}
