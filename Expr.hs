module Expr where

import Data_type
import Expr_parsing
import GHC.Float

eval :: Tree Name Value Int -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either Value String -- Result (if no errors such as missing variables)
eval vars (Get x) = lookup3 x vars
eval vars (Val x) = Left x -- for values, just give the value directly
eval vars (Add x y) = numOp2 vars (+) x y
eval vars (Sub x y) = numOp2 vars (-) x y
eval vars (Mul x y) = numOp2 vars (*) x y
eval vars (Div x y) = numOp2 vars doDivision x y
eval vars (Abs x) = numOp vars abs x
eval vars (Mod x y) = numOp2 vars doMod x y
eval vars (Pow x y) = numOp2 vars doPow x y
eval vars (ToNum x) = case x of
                        Get var -> case lookup3 var vars of
                                     Left (StrVal val) -> Left $ NumVal $ Int $ toInt val
                                     _ -> Right emptyResult 
                        _ -> eval vars x
eval vars (ToString x) = Left $ StrVal $ format $ case eval vars x of
                                                    Left val -> show val
                                                    Right _ -> scopeError
eval vars (Concat x y) = Left $ StrVal $ format (go x) ++ format (go y)
                          where go x = case eval vars x of
                                         Left val -> show val
                                         Right _ -> scopeError
eval vars (If cond x y) = case eval vars cond of
                            Left (Bool val) -> if val then eval vars x else eval vars y
                            _ -> Right boolError
eval vars (Equal x y) = case (eval vars x, eval vars y) of
                          (Left xval, Left yval) -> Left (Bool $ xval == yval)
                          _ -> Right boolError
eval vars (NotEqual x y) = case eval vars (Equal x y) of
                             Left (Bool val) -> Left (Bool $ not val)
                             _ -> Right boolError
eval vars (Greater x y) = case (eval vars x, eval vars y) of
                            (Left xval, Left yval) -> Left (Bool $ xval > yval)
                            _ -> Right boolError
eval vars (GreaterEqual x y) = case eval vars (Or (Equal x y) (Greater x y)) of
                                 Left (Bool val) -> Left (Bool val)
                                 _ -> Right boolError
eval vars (Less x y) = case eval vars (GreaterEqual x y) of
                         Left (Bool val) -> Left (Bool $ not val)
                         _ -> Right boolError
eval vars (LessEqual x y) = case eval vars (Greater x y) of
                              Left (Bool val) -> Left (Bool $ not val)
                              _ -> Right boolError
eval vars (Not x) = case eval vars x of
                      Left (Bool val) -> Left (Bool $ not val)
                      _ -> Right boolError
eval vars (And x y) = case (eval vars x, eval vars y) of
                        (Left (Bool xval), Left (Bool yval)) -> Left (Bool $ xval && yval)
                        _ -> Right boolError
eval vars (Or x y) = case (eval vars x, eval vars y) of
                       (Left (Bool xval), Left (Bool yval)) -> Left (Bool $ xval || yval)
                       _ -> Right boolError

numOp :: Tree Name Value Int -> (Numeric -> Numeric) -> Expr -> Either Value String
numOp vars f x = case eval vars x of
                     Left (NumVal xval) -> Left $ NumVal (f xval)
                     _ -> Right unsupported

numOp2 :: Tree Name Value Int -> (Numeric -> Numeric -> Numeric) -> Expr -> Expr -> Either Value String
numOp2 vars f x y = case (eval vars x, eval vars y) of
                     (Left (NumVal xval), Left (NumVal yval)) -> Left $ NumVal (f xval yval)
                     _ -> Right unsupported

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

-- Get payload of a particular tuple with 3 elements, reduce the layers of Left
lookup3 :: Name -> Tree Name Value Int -> Either Value String --https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC-List.html
lookup3 name Leaf =  Right emptyResult
lookup3 name (Node lt nName nValue nScope rt)
  | nName == name = Left nValue
  | name < nName = lookup3 name lt
  | otherwise    = lookup3 name rt


-- Get a particular tuple with 3 elements
extract :: Ord a => a -> Tree a b c -> Either (a, b, c) String
extract name Leaf =  Right emptyResult--https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC-List.html
extract name (Node lt nName nValue nScope rt)
  | nName == name              = Left (nName, nValue, nScope)
  | name < nName               = extract name lt
  | otherwise                  = extract name rt


fst3 :: (a, b, c) -> a -- Get first element of a tuple with 3 elements
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b -- Get second element of a tuple with 3 elements
snd3 (_, b, _) = b

lst3 :: (a, b, c) -> c -- Get last element of a tuple with 3 elements
lst3 (_, _, c) = c