module REPL where

import Expr
import Parsing

data LState = LState { vars :: [(Name, Int)] }

initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars name val vars = (name, val) : filter (\var -> fst var /= name) vars

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
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
