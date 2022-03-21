module Expr where

import Data_type
import Expr_parsing
import GHC.Float (int2Double)
import GHC.Real (div)

eval :: [(Name, Value, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Get x) = lookup3 x vars
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = numOp2 vars (+) x y
eval vars (Sub x y) = numOp2 vars (-) x y
eval vars (Mul x y) = numOp2 vars (*) x y
eval vars (Div x y) = numOp2 vars doDivision x y
eval vars (Abs x) = numOp vars abs x
eval vars (Mod x y) = numOp2 vars doMod x y
eval vars (Pow x y) = numOp2 vars doPow x y
eval vars (ToNum x) = case x of
                        Get var -> case lookup3 var vars of
                                     Just (StrVal val) -> Just $ NumVal $ Int $ toInt val
                                     _ -> Nothing -- the main thing should be variable casting (WIP)
                        _ -> eval vars x
eval vars (ToString x) = Just $ StrVal $ format $ maybe "*Invalid/ out-of-scope expression*" show (eval vars x)
eval vars (Concat x y) = Just $ StrVal $ format (go x) ++ format (go y)
                          where go x = maybe "*Invalid/ out-of-scope expression*" show (eval vars x)
eval vars (If cond x y) = case eval vars cond of
                          Just (NumVal val) -> case val of
                                               Int int -> if int /= 0 then eval vars x else eval vars y
                                               _ -> Nothing
                          Just (Bool val) -> if val then eval vars x else eval vars y
                          _ -> Nothing
eval vars (Equal x y) = case (eval vars x, eval vars y) of
                          (Just xval, Just yval) -> Just (Bool $ xval == yval)
                          _ -> Just (Bool False)
eval vars (NotEqual x y) = case eval vars (Equal x y) of
                             Just (Bool val) -> Just (Bool $ not val)
                             _ -> Just (Bool False)
eval vars (Greater x y) = case (eval vars x, eval vars y) of
                            (Just xval, Just yval) -> Just (Bool $ xval > yval)
                            _ -> Just (Bool False)
eval vars (GreaterEqual x y) = case eval vars (Equal x y) of
                                 Just (Bool True) -> Just (Bool True)
                                 _ -> case eval vars (Greater x y) of
                                        Just (Bool val) -> Just (Bool val)
                                        _ -> Just (Bool False)
eval vars (Less x y) = case eval vars (GreaterEqual x y) of
                         Just (Bool val) -> Just (Bool $ not val)
                         _ -> Just (Bool False)
eval vars (LessEqual x y) = case eval vars (Greater x y) of
                              Just (Bool val) -> Just (Bool $ not val)
                              _ -> Just (Bool False)
eval vars (Not x) = case eval vars x of
                      Just (Bool val) -> Just (Bool $ not val)
                      _ -> Just (Bool False)
eval vars (And x y) = case (eval vars x, eval vars y) of
                        (Just (Bool xval), Just (Bool yval)) -> Just (Bool $ xval && yval)
                        _ -> Just (Bool False)
eval vars (Or x y) = case (eval vars x, eval vars y) of
                       (Just (Bool xval), Just (Bool yval)) -> Just (Bool $ xval || yval)
                       _ -> Just (Bool False)

numOp :: [(Name, Value, Int)] -> (Numeric -> Numeric) -> Expr -> Maybe Value
numOp vars f x = case eval vars x of
                     Just (NumVal xval) -> Just $ NumVal (f xval)
                     _ -> Nothing

numOp2 :: [(Name, Value, Int)] -> (Numeric -> Numeric -> Numeric) -> Expr -> Expr -> Maybe Value
numOp2 vars f x y = case (eval vars x, eval vars y) of
                     (Just (NumVal xval), Just (NumVal yval)) -> Just $ NumVal (f xval yval)
                     _ -> Nothing

doDivision :: Numeric -> Numeric -> Numeric
doDivision x y = case (x, y) of
                   (Int xval, Int yval) -> Int (quot xval yval)
                   (Int xval, Float yval) -> Float (int2Double xval / yval)
                   (Float xval, Int yval) -> Float (xval / int2Double yval)
                   (Float xval, Float yval) -> Float (xval / yval)

doMod :: Numeric -> Numeric -> Numeric
doMod x y = case (x, y) of
              (Int xval, Int yval) -> Int (mod xval yval)
              (Int xval, Float yval) -> Float (doubleMod (int2Double xval) yval)
              (Float xval, Int yval) -> Float (doubleMod xval (int2Double yval))
              (Float xval, Float yval) -> Float (doubleMod xval yval)

--https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:round
doubleMod :: Double -> Double -> Double
doubleMod x y = x - (y * int2Double (floor (x / y)))

doPow :: Numeric -> Numeric -> Numeric
doPow x y = case (x, y) of
              (Int xval, Int yval) -> Int (xval ^ yval)
              (Int xval, Float yval) -> Float (int2Double xval ** yval)
              (Float xval, Int yval) -> Float (xval ^ yval)
              (Float xval, Float yval) -> Float (xval ** yval)

format :: String -> String --https://stackoverflow.com/questions/3740621/removing-string-double-quotes-in-haskell
format s@[c]                     = s
format ('"':s)  | last s == '"'  = init s
                | otherwise      = s
format ('\'':s) | last s == '\'' = init s
                | otherwise      = s
format s                         = s

lookup3 :: (Eq a) => a -> [(a,b,c)] -> Maybe b --https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC-List.html
lookup3 _key [] =  Nothing
lookup3  key ((x,y,_):xys)
    | key == x  =  Just y
    | otherwise =  lookup3 key xys

extract key [] =  Nothing--https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC-List.html
extract key ((x,y,z):xys)
  | key == x  =  Just (x,y,z)
  | otherwise =  extract key xys

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

lst3 :: (a, b, c) -> c
lst3 (_, _, c) = c