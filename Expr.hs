module Expr where

import Expr_parsing
import Data_type
import GHC.Float (int2Double)
import GHC.Real (div)

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Get x) = lookup x vars
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = numOp2 vars (+) x y
eval vars (Sub x y) = numOp2 vars (-) x y
eval vars (Mul x y) = numOp2 vars (*) x y
eval vars (Div x y) = numOp2 vars doDivision x y
eval vars (Abs x) = numOp vars abs x
eval vars (Mod x y) = numOp2 vars doMod x y
eval vars (Pow x y) = numOp2 vars doPow x y
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

numOp :: [(Name, Value)] -> (Numeric -> Numeric) -> Expr -> Maybe Value
numOp vars f x = case eval vars x of
                     Just (NumVal xval) -> Just $ NumVal (f xval)
                     _ -> Nothing
numOp2 :: [(Name, Value)] -> (Numeric -> Numeric -> Numeric) -> Expr -> Expr -> Maybe Value
numOp2 vars f x y = case eval vars x of
                     Just (NumVal xval) -> case eval vars y of
                            Just (NumVal yval) -> Just $ NumVal (f xval yval)
                            _ -> Nothing
                     _ -> Nothing

format :: String -> String --https://stackoverflow.com/questions/3740621/removing-string-double-quotes-in-haskell
format s@[c]                     = s
format ('"':s)  | last s == '"'  = init s
                | otherwise      = s
format ('\'':s) | last s == '\'' = init s
                | otherwise      = s
format s                         = s

doDivision :: Numeric -> Numeric -> Numeric
doDivision x y = case x of
       Int xval -> case y of
                     Int yval -> Int (quot xval yval)
                     Float yval -> Float (int2Double xval / yval)
       Float xval -> case y of
                     Int yval -> Float (xval / int2Double yval)
                     Float yval -> Float (xval / yval)

doMod :: Numeric -> Numeric -> Numeric
doMod x y = case x of
       Int xval -> case y of
                     Int yval -> Int (mod xval yval)
                     Float yval -> Float (doubleMod (int2Double xval) yval)
       Float xval -> case y of
                     Int yval -> Float (doubleMod xval (int2Double yval))
                     Float yval -> Float (doubleMod xval yval)

--https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:round
doubleMod :: Double -> Double -> Double
doubleMod x y = x - (y * int2Double (floor (x / y)))

doPow :: Numeric -> Numeric -> Numeric
doPow x y = case x of
       Int xval -> case y of
              Int yval -> Int (xval ^ yval)
              Float yval -> Float (int2Double xval ** yval)
       Float xval -> case y of
              Int yval -> Float (xval ^ yval)
              Float yval -> Float (xval ** yval)