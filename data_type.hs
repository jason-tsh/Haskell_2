module Data_type where

import GHC.Float (int2Double)
import GHC.Real (div)

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Cond Expr [Command] [Command]
             | Repeat Int [Command]
             | While Expr [Command]
             | DoWhile Expr [Command]
             | For [Command] Expr [Command] [Command]
             | Read Name
             | SetFunc Name Int [Command]
             | Func Name [Expr]
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
          | Equal Expr Expr
          | NotEqual Expr Expr
          | Greater Expr Expr
          | GreaterEqual Expr Expr
          | Less Expr Expr
          | LessEqual Expr Expr
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
  deriving Show

data Value = NumVal Numeric | StrVal String | Bool Bool
  deriving Ord

instance Eq Value where
  (==) (NumVal x) (NumVal y) = x == y
  (==) (NumVal x) _ = False
  (==) (StrVal x) (StrVal y) = x == y
  (==) (StrVal x) _ = False
  (==) (Bool x) (Bool y) = x == y
  (==) (Bool x) _ = False
  (/=) x y = not $ x == y

instance Show Value where
  show (NumVal (Int val)) = show val
  show (NumVal (Float val)) = show val
  show (StrVal val) = show val
  show (Bool val) = show val

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