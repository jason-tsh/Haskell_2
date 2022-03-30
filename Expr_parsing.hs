module Expr_parsing where

import Parsing
import Data_type

pBatch :: Parser [Command]
pBatch = do many pComment
            do fst <- space *> many1 pCommand <* space
               rest <- pBatch
               return (fst ++ rest)
             ||| return []

pComment :: Parser ()
pComment = symbol "--" *> many (sat (/= '\n')) *> space

pCommand :: Parser Command
pCommand = pFunc ||| pCond ||| pSet
            ||| do Read <$> (symbol "read" *> many (sat (/= '\"')))
            ||| do Print <$> (symbol "print" *> pExpr)
            ||| do symbol "quit" >> return Quit

pFunc :: Parser Command
pFunc = do name <- symbol "void" *> many1 (letter ||| char '_')
           argv <- symbol "(" *> pHead (many1 (letter ||| char '_')) <* symbol ")"
           SetFunc name argv <$> pBody
         ||| do name <- many1 (letter ||| char '_')
                argv <- symbol "(" *> pHead pExpr <* symbol ")"
                return $ Func name argv

pSet :: Parser Command
pSet = do t <- many1 (letter ||| char '_')
          Set t <$> (symbol "=" *> pExpr)

pCond :: Parser Command
pCond = do cond <- symbol "if" *> symbol "(" *> pBools <* symbol ")"
           true <- pBody
           do Cond cond true <$> (symbol "else" *> pBody)
            ||| return (Cond cond true [])
         ||| do acc <- symbol "repeat" *> many1 digit
                Repeat (toInt acc) <$> pBody
         ||| do cond <- symbol "while" *> symbol "(" *> pBools <* symbol ")"
                While cond <$> pBody
         ||| do cmd <- symbol "do" *> pBody
                cond <- symbol "while" *> symbol "(" *> pBools <* symbol ")"
                return $ DoWhile cond cmd
         ||| do init <- symbol "for" *> symbol "(" *> pHead pSet <* symbol ";"
                cond <- pBools <* symbol ";"
                after <- pHead pSet <* symbol ")"
                For init cond after <$> pBody

pHead :: Parser a -> Parser [a]
pHead p = do fst <- p
             rest <- many (symbol "," *> p)
             return (fst:rest)
           ||| return []

pBody :: Parser [Command]
pBody = do symbol "{" *> many pComment
           do fst <- pCommand
              rest <- many (many (symbol ";") *> many pComment *> pCommand)
              many pComment *> symbol "}"
              return (fst:rest)
            ||| do symbol "}" >> return []

pBools :: Parser Expr
pBools = do Not <$> (symbol "!" *> symbol "(" *> (pBools ||| pUrgent pBools) <* symbol ")")
          ||| do fst <- pBool ||| pUrgent pBools
                 do And fst <$> (symbol "&&" *> pBools)
                  ||| do Or fst <$> (symbol "||" *> pBools)
                  ||| return fst

pBool :: Parser Expr
pBool = do fst <- pNumOp
           do Equal fst <$> (symbol "==" *> pNumOp)
            ||| do NotEqual fst <$> (symbol "/=" *> pNumOp)
            ||| do GreaterEqual fst <$> (symbol ">=" *> pNumOp)
            ||| do Greater fst <$> (symbol ">=" *> pNumOp)
            ||| do LessEqual fst <$> (symbol "<=" *> pNumOp)
            ||| do Less fst <$> (symbol "<" *> pNumOp)
            ||| return fst

pExpr :: Parser Expr
pExpr = do Abs <$> (symbol "abs" *> pNum) -- haskell syntax
         ||| do cond <- symbol "if" *> pBools
                true <- symbol "then" *> pExpr
                symbol "else" >> If cond true <$> pExpr
         ||| pCast
         ||| pArith
              ||| do t <- pTerm
                     do Concat t <$> (symbol "++" *> pExpr)
                      ||| return t

pNumOp :: Parser Expr
pNumOp = pArith ||| pTerm

pCast :: Parser Expr
pCast = do symbol "toNum" <* symbol "("
           do ToNum <$> (char '\"' *> pNum <* char '\"' <* symbol ")")
            ||| do ToNum . Get <$> (many1 (letter ||| char '_') <* symbol ")")-- variable
         ||| do ToString <$> (symbol "toStr" *> symbol "(" *> pExpr <* symbol ")")

pArith :: Parser Expr
pArith = do t <- pTerm
            do Add t <$> (symbol "+" *> pExpr)
             ||| do Sub t <$> (symbol "-" *> pExpr)
             ||| do Pow t <$> (symbol "^" *> pExpr)
             ||| do Mod t <$> (symbol "mod" *> pExpr)

pFactor :: Parser Expr
pFactor = pNum ||| pVar ||| pStr ||| pUrgent pExpr

pNum :: Parser Expr
pNum = pFloat ||| pInt

pVar :: Parser Expr
pVar = do Val . StrVal <$> symbol "input" -- input keyword for user input
        ||| do Get <$> (many1 (letter ||| char '_') <* space) -- variable

pStr :: Parser Expr
pStr = Val . StrVal <$> (char '\"' *> many (sat (/= '\"')) <* char '\"' <* space) -- string (empty string is possible)

pUrgent :: Parser p -> Parser p
pUrgent p = symbol "(" *> p <* symbol ")" --- expression with priority

pInt :: Parser Expr
pInt = do d <- symbol "(" *> symbol "-" *> many1 digit <* symbol ")"
          return (Val $ NumVal $ Int $ negate $ toInt d) -- negative integer
        ||| do d <- many1 digit <* space
               return (Val $ NumVal $ Int $ toInt d) -- positive integer

pFloat :: Parser Expr
pFloat = do d <- symbol "(" *> symbol "-" *> many1 digit
            f <- symbol "." *> many1 digit <* symbol ")"
            return (Val $ NumVal $ Float $ negate $ read $ d <> "." <> f) -- negative float
          ||| do d <- many1 digit <* symbol "."
                 f <- many1 digit <* space
                 return (Val $ NumVal $ Float $ read $ d <> "." <> f) -- positive float

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do Mul f <$> (symbol "*" *> pExpr)
            ||| do Div f <$> (symbol "/" *> pExpr)
            ||| return f

toInt :: String -> Int
toInt = go 0
  where go acc [] = acc
        go acc (x:xs) | 0 <= val && val <= 9 = go (10 * acc + val) xs
                      | otherwise = 0
          where val = digit2Int x

digit2Int :: Char -> Int
digit2Int x = fromEnum x - fromEnum '0'