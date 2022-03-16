module REPL where

import Expr
import Parsing

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

process :: LState -> Command -> IO ()
process st (Set var e)
     = do print (Set var e)
          let st' = case eval list e of
                    Just val -> st {vars = updateVars var val list}
                    Nothing -> st
                    where list = vars st
          -- st' should include the variable set to the result of evaluating e
          repl st'
process st (Print e)
     = do case eval (vars st) e of
                    Just val -> print val
                    Nothing -> putStrLn "No entry found"
          -- Print the result of evaluation
          repl st
process st (Cond cond x y)
     = do case eval list cond of
            Just (NumVal val) -> case val of
                                   Int int -> if int /= 0 then process st x
                                                          else process st y
                                   _ -> putStrLn "No entry found"
            _ -> putStrLn "No entry found"
          repl st
          where list = vars st
process st (Repeat acc cmd)
     = do if acc > 0 then putStrLn "--Repeat loop exits--"
                     else case cmd of
                          [] -> process st (Repeat (acc-1) cmd)
                          (x:xs) -> do process st x
                                       process st (Repeat acc xs)
          repl st
process st Quit = return ()

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do putStr "> "
             inp <- getLine
             case parse pCommand inp of
                    [(cmd, "")] -> -- Must parse entire input
                                   process st cmd
                    _ -> do putStrLn "Parse error"
                            repl st
