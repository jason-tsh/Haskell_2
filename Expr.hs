module Expr where

import Expr_parsing
import Parsing
import GHC.Float (int2Double)
import GHC.Real (div)

type Name = String

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
       (+) (Int x) (Float y) = Float ((int2Double x) + y)
       (+) (Float x) (Int y) = Float (x + (int2Double y))
       (+) (Float x) (Float y) = Float (x + y)
       (-) (Int x) (Int y) = Int (x - y)
       (-) (Int x) (Float y) = Float ((int2Double x) - y)
       (-) (Float x) (Int y) = Float (x - (int2Double y))
       (-) (Float x) (Float y) = Float (x - y)
       (*) (Int x) (Int y) = Int (x * y)
       (*) (Int x) (Float y) = Float ((int2Double x) * y)
       (*) (Float x) (Int y) = Float (x * (int2Double y))
       (*) (Float x) (Float y) = Float (x * y)
       abs (Int x) = Int (abs x)
       abs (Float x) = Float (abs x)
       signum (Int x) = Int (signum x)
       signum (Float x) = Float (signum x)
       fromInteger x = Int (fromInteger x)

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
             | While Expr [Command]
             | DoWhile Expr [Command]
             | For [Command] Expr [Command] [Command]
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
                     Float yval -> Float ((int2Double xval) / yval)
       Float xval -> case y of
                     Int yval -> Float (xval / (int2Double yval))
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
doubleMod x y = x - (y * (int2Double $ floor (x / y)))

doPow :: Numeric -> Numeric -> Numeric
doPow x y = case x of
       Int xval -> case y of
              Int yval -> Int (xval ^ yval)
              Float yval -> Float ((int2Double xval) ** yval)
       Float xval -> case y of
              Int yval -> Float (xval ^ yval)
              Float yval -> Float (xval ** yval)

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

toInt :: String -> Int
toInt = go 0
  where go acc [] = acc
        go acc (x:xs) | 0 <= val && val <= 9 = go (10 * acc + val) xs
                      | otherwise = 0
          where val = digitToInt x

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = pSet ||| pCond
            ||| do symbol "print"
                   Print <$> pExpr
            ||| do symbol "quit"
                   return Quit

pSet :: Parser Command
pSet = do t <- many1 letter
          symbol "="
          Set t <$> pExpr

pCond :: Parser Command
pCond = do symbol "if"
           cond <- pExpr
           symbol "then"
           true <- pCommand
           symbol "else"
           Cond cond true <$> pCommand
         ||| do symbol "repeat"
                acc <- many1 digit
                Repeat (toInt acc) <$> pBody
         ||| do symbol "while"
                cond <- pExpr
                While cond <$> pBody
         ||| do symbol "do"
                cmd <- pBody
                symbol "while" *> symbol "("
                cond <- pExpr
                symbol ")"
                return $ DoWhile cond cmd
         ||| do symbol "for"
                init <- pHead pSet <* symbol ";"
                cond <- pExpr <* symbol ";"
                after <- pHead pSet
                For init cond after <$> pBody

pHead :: Parser a -> Parser [a]
pHead p = do fst <- p
             rest <- many (symbol "," *> p)
             return (fst:rest)
           ||| return []

pBody :: Parser [Command]
pBody = do symbol "{"
           fst <- pCommand
           rest <- many (symbol ";" *> pCommand)
           symbol "}"
           return (fst:rest)

pExpr :: Parser Expr
pExpr = do symbol "abs"
           Abs <$> pNum -- haskell syntax
         ||| do symbol "toInt("
                do char '\"'
                   n <- pNum
                   char '\"' *> symbol ")"
                   return $ ToNum n
                 ||| do v <- many1 letter
                        symbol ")"
                        return $ ToNum $ Get v -- variable
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
pFactor = pNum ||| pVar ||| pStr ||| pUrgent

pNum :: Parser Expr
pNum = pFloat ||| pInt

pVar :: Parser Expr
pVar = do symbol "input"
          return (Val $ StrVal "input") -- input keyword for user input
        ||| do v <- many1 letter
               space
               return $ Get v -- variable

pStr :: Parser Expr
pStr = do char '\"'
          v <- many $ sat (/= '\"')
          char '\"'
          space
          return (Val $ StrVal v) -- string (empty string is possible)

pUrgent :: Parser Expr
pUrgent = do symbol "("
             e <- pExpr
             symbol ")"
             return e --- expression with priority

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
              Mul f <$> pExpr
            ||| do symbol "/"
                   Div f <$> pExpr
            ||| return f
