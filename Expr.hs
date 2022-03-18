module Expr where

import Expr_parsing

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Get x) = lookup x vars
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = numOp2 vars (+) x y
eval vars (Sub x y) = numOp2 vars (-) x y
eval vars (Mul x y) = numOp2 vars (*) x y
eval vars (Div x y) = numOp2 vars quot x y
eval vars (Abs x) = numOp vars abs x
eval vars (Mod x y) = numOp2 vars mod x y
eval vars (Pow x y) = numOp2 vars (^) x y
eval vars (ToNum x) = case x of
                        Get var -> case lookup var vars of
                                     Just (StrVal val) -> Just $ NumVal $ Int $ toInt val
                                     _ -> Nothing -- the main thing should be variable casting (WIP)
                        _ -> eval vars x
eval vars (ToString x) = Just $ StrVal $ format $ maybe "*Invalid expression*" show (eval vars x)
eval vars (Concat x y) = Just $ StrVal $ format (go x) ++ format (go y)
                          where go x = maybe "*Invalid expression*" show (eval vars x)
eval vars (If cond x y) = case eval vars cond of
                          Just (NumVal val) -> case val of
                                               Int int -> if int /= 0
                                                          then eval vars x
                                                          else eval vars y
                                               _ -> Nothing
                          _ -> Nothing

format :: String -> String --https://stackoverflow.com/questions/3740621/removing-string-double-quotes-in-haskell
format s@[c]                     = s
format ('"':s)  | last s == '"'  = init s
                | otherwise      = s
format ('\'':s) | last s == '\'' = init s
                | otherwise      = s
format s                         = s

numOp :: [(Name, Value)] -> (Int -> Int) -> Expr -> Maybe Value
numOp vars f x = case eval vars x of
                   Just (NumVal (Int xval)) -> Just $ NumVal $ Int (f xval)
                   _ -> Nothing

numOp2 :: [(Name, Value)] -> (Int -> Int -> Int) -> Expr -> Expr -> Maybe Value
numOp2 vars f x y = case eval vars x of
                      Just (NumVal (Int xval)) -> case eval vars y of
                                     Just (NumVal (Int yval)) -> Just $ NumVal $ Int (f xval yval)
                                     _ -> Nothing
                      _ -> Nothing