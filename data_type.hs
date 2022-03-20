module Data_type where

import GHC.Float (int2Double)
import GHC.Real (div)

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Cond Expr Command Command
             | Repeat Int [Command]
             | While Expr [Command]
             | DoWhile Expr [Command]
             | For [Command] Expr [Command] [Command]
             | Quit
  deriving Show

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Abs Expr
          | Mod Expr Expr
          | Pow Expr Expr
          | ToNum Expr
          | ToString Expr
          | Concat Expr Expr
          | Val Value
          | Get Name
          | If Expr Expr Expr
  deriving Show

data Value = NumVal Numeric | StrVal String

instance Show Value where
  show (NumVal (Int val)) = show val
  show (NumVal (Float val)) = show val
  show (StrVal val) = show val

data Numeric = Int Int | Float Double
  deriving Show

--https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-Float.html#v:int2Double
instance Num Numeric where
       (+) (Int x) (Int y) = Int (x + y)
       (+) (Int x) (Float y) = Float (int2Double x + y)
       (+) (Float x) (Int y) = Float (x + int2Double y)
       (+) (Float x) (Float y) = Float (x + y)
       (-) (Int x) (Int y) = Int (x - y)
       (-) (Int x) (Float y) = Float (int2Double x - y)
       (-) (Float x) (Int y) = Float (x - int2Double y)
       (-) (Float x) (Float y) = Float (x - y)
       (*) (Int x) (Int y) = Int (x * y)
       (*) (Int x) (Float y) = Float (int2Double x * y)
       (*) (Float x) (Int y) = Float (x * int2Double y)
       (*) (Float x) (Float y) = Float (x * y)
       abs (Int x) = Int (abs x)
       abs (Float x) = Float (abs x)
       signum (Int x) = Int (signum x)
       signum (Float x) = Float (signum x)
       fromInteger x = Int (fromInteger x)

instance Eq Numeric where
  (==) (Int x) (Int y) = x == y
  (==) (Float x) (Int y) = x == int2Double y
  (==) (Int x) (Float y) = int2Double x == y
  (==) (Float x) (Float y) = x == y
  (/=) x y = not $ x == y

instance Ord Numeric where
  (<=) (Int x) (Int y)= x <= y
  (<=) (Float x) (Int y) = x <= int2Double y
  (<=) (Int x) (Float y) = int2Double x <= y
  (<=) (Float x) (Float y) = x <= y
  (<) x y = x <= y && x /= y
  (>=) x y = not $ x < y
  (>) x y = not $ x <= y

type Name = String