module Expr where

import Data_type
import Expr_parsing
import GHC.Float

eval :: Tree Name Value Int -> -- Variable name to value mapping
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
                                     _ -> Nothing
                        _ -> eval vars x
eval vars (ToString x) = Just $ StrVal $ format $ maybe "*Invalid/ out-of-scope expression*" show (eval vars x)
eval vars (Concat x y) = Just $ StrVal $ format (go x) ++ format (go y)
                          where go x = maybe "*Invalid/ out-of-scope expression*" show (eval vars x)
eval vars (If cond x y) = case eval vars cond of
                            Just (Bool val) -> if val then eval vars x else eval vars y
                            _ -> Nothing
eval vars (Equal x y) = case (eval vars x, eval vars y) of
                          (Just xval, Just yval) -> Just (Bool $ xval == yval)
                          _ -> Nothing
eval vars (NotEqual x y) = case eval vars (Equal x y) of
                             Just (Bool val) -> Just (Bool $ not val)
                             _ -> Nothing
eval vars (Greater x y) = case (eval vars x, eval vars y) of
                            (Just xval, Just yval) -> Just (Bool $ xval > yval)
                            _ -> Nothing
eval vars (GreaterEqual x y) = case eval vars (Or (Equal x y) (Greater x y)) of
                                 Just (Bool val) -> Just (Bool val)
                                 _ -> Nothing
eval vars (Less x y) = case eval vars (GreaterEqual x y) of
                         Just (Bool val) -> Just (Bool $ not val)
                         _ -> Nothing
eval vars (LessEqual x y) = case eval vars (Greater x y) of
                              Just (Bool val) -> Just (Bool $ not val)
                              _ -> Nothing
eval vars (Not x) = case eval vars x of
                      Just (Bool val) -> Just (Bool $ not val)
                      _ -> Nothing
eval vars (And x y) = case (eval vars x, eval vars y) of
                        (Just (Bool xval), Just (Bool yval)) -> Just (Bool $ xval && yval)
                        _ -> Nothing
eval vars (Or x y) = case (eval vars x, eval vars y) of
                       (Just (Bool xval), Just (Bool yval)) -> Just (Bool $ xval || yval)
                       _ -> Nothing

numOp :: Tree Name Value Int -> (Numeric -> Numeric) -> Expr -> Maybe Value
numOp vars f x = case eval vars x of
                     Just (NumVal xval) -> Just $ NumVal (f xval)
                     _ -> Nothing

numOp2 :: Tree Name Value Int -> (Numeric -> Numeric -> Numeric) -> Expr -> Expr -> Maybe Value
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

-- Get payload of a particular tuple with 3 elements, reduce the layers of Just
lookup3 :: Name -> Tree Name Value Int -> Maybe Value --https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC-List.html
lookup3 name Leaf =  Nothing
lookup3 name (Node lt nName nValue nScope rt)
  | nName == name = Just nValue
  | name < nName = lookup3 name lt
  | otherwise    = lookup3 name rt


-- Get a particular tuple with 3 elements
extract :: Ord a => a -> Tree a b c -> Maybe (a, b, c)
extract name Leaf =  Nothing--https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC-List.html
extract name (Node lt nName nValue nScope rt)
  | nName == name              = Just (nName, nValue, nScope)
  | name < nName               = extract name lt
  | otherwise                  = extract name rt


fst3 :: (a, b, c) -> a -- Get first element of a tuple with 3 elements
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b -- Get second element of a tuple with 3 elements
snd3 (_, b, _) = b

lst3 :: (a, b, c) -> c -- Get last element of a tuple with 3 elements
lst3 (_, _, c) = c