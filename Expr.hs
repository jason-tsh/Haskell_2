module Expr where

import Parsing

type Name = String

data Value = NumVal Numeric | StrVal String

instance Show Value where
  show (NumVal (Int val)) = show val
  show (NumVal (Float val)) = show val
  show (StrVal val) = show val

data Numeric = Int Int | Float Double
  deriving Show

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | ToInt Expr
          | ToString Expr
          | Concat Expr Expr
          | Val Value
          | Get Name
          | If Expr Expr Expr
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Get x) = lookup x vars
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = numOp vars (+) x y
eval vars (Sub x y) = numOp vars (-) x y
eval vars (Mul x y) = numOp vars (*) x y
eval vars (Div x y) = numOp vars quot x y
eval vars (ToInt x) = Just $ NumVal $ Int $ toInt $ show x
eval vars (ToString x) = Just $ StrVal $ show x
eval vars (Concat x y) = Just $ StrVal $ go x ++ go y
                          where go x = maybe "**Error**" show (eval vars x)
eval vars (If cond x y) = case eval vars cond of
                            Just (NumVal val) -> case val of
                                                 Int int -> if int /= 0
                                                            then eval vars x
                                                            else eval vars y
                                                 Float float -> if float /= 0.0
                                                            then eval vars x
                                                            else eval vars y
                            _ -> Nothing

numOp :: [(Name, Value)] -> (Int -> Int -> Int) -> Expr -> Expr -> Maybe Value
numOp vars f x y = case eval vars x of
                      Just (NumVal (Int xval)) -> case eval vars y of
                                     Just (NumVal (Int yval)) -> Just $ NumVal $ Int (f xval yval)
                                     _ -> Nothing
                      _ -> Nothing

toInt :: String -> Int
toInt = go 0
  where go acc [] = acc
        go acc (x:xs) | 0 <= val && val <= 9 = go (10 * acc + val) xs
                      | otherwise = 0
          where val = digitToInt x

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do t <- many1 letter
              symbol "="
              Set t <$> pExpr
            ||| do symbol "print"
                   Print <$> pExpr
                 ||| do symbol "quit"
                        return Quit

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do symbol "++"
              Concat t <$> pExpr
            ||| do symbol "+"
                   Add t <$> pExpr
            ||| do symbol "-"
                   Sub t <$> pExpr
            ||| return t
         ||| do symbol "toInt("
                char '\"'
                n <- pNum
                char '\"'
                symbol ")"
                return $ ToInt n
         ||| do symbol "if"
                cond <- pExpr
                symbol "then"
                true <- pExpr
                symbol "else"
                false <- pExpr
                If cond true <$> pExpr       

pFactor :: Parser Expr
pFactor = do pNum
           ||| do v <- many1 letter
                  return $ Get v -- variable
           ||| do char '\"'
                  v <- many $ sat (/= '\"')
                  char '\"'
                  return (Val $ StrVal v) -- string (empty string is possible)
           ||| do symbol "("
                  e <- pExpr
                  symbol ")"
                  return e

pNum :: Parser Expr
pNum = do string "(-"
          d <- many1 digit
          do char ')'
             return (Val $ NumVal $ Int $ negate $ toInt d) -- negative integer
           ||| do char '.'
                  f <- many1 digit
                  char ')'
                  return (Val $ NumVal $ Float $ negate $ read $ d <> "." <> f) -- negative float
         ||| do d <- many1 digit
                do char '.'
                   f <- many1 digit
                   return (Val $ NumVal $ Float $ read $ d <> "." <> f) -- positive float
                 ||| return (Val $ NumVal $ Int $ toInt d) -- positive integer

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"
              Mul f <$> pTerm
            ||| do symbol "/"
                   Div f <$> pTerm
                 ||| return f
