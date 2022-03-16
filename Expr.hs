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

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Cond Expr Command Command
             | Repeat Int [Command]
             | Quit
  deriving Show

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
eval vars (ToNum x) = case x of -- the main thing should be variable casting (WIP)
                        (Val e) -> Just e
                        _ -> Nothing
eval vars (ToString x) = Just $ StrVal $ show x
eval vars (Concat x y) = Just $ StrVal $ go x ++ go y
                          where go x = maybe "**Error**" show (eval vars x)
eval vars (If cond x y) = case eval vars cond of
                          Just (NumVal val) -> case val of
                                               Int int -> if int /= 0
                                                          then eval vars x
                                                          else eval vars y
                                               _ -> Nothing
                          _ -> Nothing

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
            ||| do symbol "if"
                   cond <- pExpr
                   symbol "then"
                   true <- pCommand
                   symbol "else"
                   Cond cond true <$> pCommand
            ||| do symbol "repeat"
                   acc <- many1 digit
                   symbol "{"
                   cmd <- many (many (symbol ";") *> pCommand)
                   return $ Repeat (toInt acc) cmd
            ||| do symbol "quit"
                   return Quit

pExpr :: Parser Expr
pExpr = do symbol "abs"
           Abs <$> pNum -- haskell syntax
         ||| do symbol "toInt("
                char '\"'
                n <- pInt
                char '\"'
                symbol ")"
                return $ ToNum n
         ||| do symbol "toString("
                n <- pExpr
                symbol ")"
                return $ ToString n
         ||| do symbol "if"
                cond <- pExpr
                symbol "then"
                true <- pExpr
                symbol "else"
                If cond true <$> pExpr
         ||| do t <- pTerm
                do symbol "++"
                   Concat t <$> pExpr
                 ||| do symbol "+"
                        Add t <$> pExpr
                 ||| do symbol "-"
                        Sub t <$> pExpr
                 ||| do symbol "^"
                        Pow t <$> pExpr
                 ||| do symbol "mod"
                        Mod t <$> pExpr
                 ||| return t

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
                  return e --- expression with priority

pNum :: Parser Expr
pNum = pFloat ||| pInt

pInt :: Parser Expr
pInt = do symbol "(-"
          d <- many1 digit
          symbol ")"
          space
          return (Val $ NumVal $ Int $ negate $ toInt d) -- negative integer
        ||| do d <- many1 digit
               space
               return (Val $ NumVal $ Int $ toInt d) -- positive integer

pFloat :: Parser Expr
pFloat = do symbol "(-"
            d <- many1 digit
            char '.'
            f <- many1 digit
            symbol ")"
            return (Val $ NumVal $ Float $ negate $ read $ d <> "." <> f) -- negative float
          ||| do d <- many1 digit
                 char '.'
                 f <- many1 digit
                 space
                 return (Val $ NumVal $ Float $ read $ d <> "." <> f) -- positive float

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"
              Mul f <$> pTerm
            ||| do symbol "/"
                   Div f <$> pTerm
            ||| return f
