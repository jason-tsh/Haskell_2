module Expr where

import Parsing

type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr          
          | ToString Expr
          | Val Int
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

eval :: [(Name, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Int -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = case eval vars x of
                        Just xval -> case eval vars y of
                                       Just yval -> Just $ xval + yval
                                       Nothing -> Nothing
                        Nothing -> Nothing -- return an error (because it's not implemented yet!)
eval vars (Sub x y) = case eval vars x of
                        Just xval -> case eval vars y of
                                       Just yval -> Just $ xval - yval
                                       Nothing -> Nothing
                        Nothing -> Nothing -- return an error (because it's not implemented yet!)
eval vars (Mul x y) = case eval vars x of
                        Just xval -> case eval vars y of
                                       Just yval -> Just $ xval * yval
                                       Nothing -> Nothing
                        Nothing -> Nothing -- return an error (because it's not implemented yet!)
eval vars (Div x y) = case eval vars x of
                        Just xval -> case eval vars y of
                                       Just yval -> Just $ xval `div` yval
                                       Nothing -> Nothing
                        Nothing -> Nothing -- return an error (because it's not implemented yet!)
eval vars (ToString x) = Nothing

toInt :: String -> Int
toInt = go 0
  where go acc [] = acc
        go acc (x:xs) | 0 <= val && val <= 9 = go (10 * acc + val) xs
                      | otherwise = 0
          where val = digitToInt x

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do t <- letter
              space
              char '='
              space
              Set [t] <$> pExpr
            ||| do string "print"
                   space
                   Print <$> pExpr

pExpr :: Parser Expr
pExpr = do t <- pTerm
           space
           do char '+'
              space
              Add t <$> pExpr
            ||| do char '-'
                   space
                   e <- pExpr
                   error "Subtraction not yet implemented!"
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- many digit
             return (Val (toInt d))
          ||| do char '-'
                 n <- many digit
                 return (Val (-toInt n))
           ||| do v <- letter
                  error "Variables not yet implemented"
                ||| do char '('
                       space
                       e <- pExpr
                       space
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           space
           do char '*'
              space
              t <- pTerm
              error "Multiplication not yet implemented"
            ||| do char '/'
                   space
                   t <- pTerm
                   error "Division not yet implemented"
                 ||| return f
