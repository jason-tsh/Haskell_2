module Expr_parsing where

import Parsing
import Data_type

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

toInt :: String -> Int
toInt = go 0
  where go acc [] = acc
        go acc (x:xs) | 0 <= val && val <= 9 = go (10 * acc + val) xs
                      | otherwise = 0
          where val = digit2Int x

digit2Int :: Char -> Int
digit2Int x = fromEnum x - fromEnum '0'