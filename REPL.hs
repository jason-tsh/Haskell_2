module REPL where

import Expr
import Parsing
import Data.Maybe

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

process :: LState -> Command -> Bool -> IO ()
process st (Set var e) subr
     = do print (eval list e)
          let st' = case eval list e of
                    Just val -> st {vars = updateVars var val list}
                    Nothing -> st
          -- st' should include the variable set to the result of evaluating e
          if isNothing (eval list e) then putStrLn "Referred data not found, action aborted"
                                     else putStr ""
          if subr then return () else repl st'
          where list = vars st
process st (Print e) subr
     = do print (Print e)
          case eval (vars st) e of
                    Just val -> print val
                    Nothing -> putStrLn "No entry found"
          -- Print the result of evaluation
          if subr then return () else repl st
process st (Cond cond x y) subr
     = do print (Cond cond x y)
          case eval list cond of
            Just (NumVal val) -> case val of
                                   Int int -> if int /= 0 then process st x True 
                                                          else process st y True 
                                   _ -> putStrLn "Non-deterministic condition, action aborted"
            _ -> putStrLn "Non-deterministic condition, action aborted"
          if subr then return () else repl st
          where list = vars st
process st (Repeat acc cmd) subr
     = do print (Repeat acc cmd)
          if acc > 0 then case cmd of
                          (x:xs) -> do process st x True 
                                       process st (Repeat acc xs) True 
                          [] -> return ()
                     else putStrLn "--Repeat loop exits--"
          if acc > 0 && not subr then process st (Repeat (acc-1) cmd) False
                                 else if acc <= 0 && not subr then repl st
                                                              else return ()
process st (While cond cmd) subr
     = do print (While cond cmd)
          process st (Cond cond (Repeat 1 cmd) (Print $ Val $ StrVal "--While loop exits--")) True
          process st (While cond cmd) True
          if subr then return () else repl st
process st (DoWhile cond cmd) subr
     = do print (DoWhile cond cmd)
          process st (Repeat 1 cmd) True -- Do part
          process st (While cond cmd) True -- While part
          if subr then return () else repl st
process st (For init cond delta cmd) subr
     = do undefined 
process st Quit subr = return ()

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do putStr "> "
             inp <- getLine
             case parse pCommand inp of
                    [(cmd, "")] -> -- Must parse entire input
                                   process st cmd False
                    _ -> do putStrLn "Parse error"
                            repl st
