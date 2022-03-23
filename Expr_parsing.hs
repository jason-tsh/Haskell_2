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
pComment = symbol "--" >> many (sat (/= '\n')) >> space >> return ()

pCommand :: Parser Command
pCommand = pSet ||| pCond
            ||| do symbol "read"
                   do file <- many $ sat (/= '\"')
                      return $ Read file
                 ||| do symbol "input"
                        return $ Read "input"
            ||| do symbol "print"
                   Print <$> pExpr
            ||| do symbol "quit"
                   return Quit

pSet :: Parser Command
pSet = do t <- many1 letter
          symbol "="
          Set t <$> pExpr

pCond :: Parser Command
pCond = do symbol "if" *> symbol "("
           cond <- pBools <* symbol ")"
           true <- pBody
           do symbol "else"
              Cond cond true <$> pBody
            ||| return (Cond cond true [])
         ||| do symbol "repeat"
                acc <- many1 digit
                Repeat (toInt acc) <$> pBody
         ||| do symbol "while" *> symbol "("
                cond <- pBools <* symbol ")"
                While cond <$> pBody
         ||| do symbol "do"
                cmd <- pBody
                symbol "while" *> symbol "("
                cond <- pBools
                symbol ")"
                return $ DoWhile cond cmd
         ||| do symbol "for" *> symbol "("
                init <- pHead pSet <* symbol ";"
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
            ||| do symbol "}"
                   return []

pBools :: Parser Expr
pBools = do symbol "!" >> symbol "("
            fst <- pBools ||| pUrgent pBools
            symbol ")"
            return $ Not fst
          ||| do fst <- pBool ||| pUrgent pBools
                 do symbol "&&" >> And fst <$> pBools
                  ||| do symbol "||" >> Or fst <$> pBools
                  ||| return fst

pBool :: Parser Expr
pBool = do fst <- pNumOp
           do symbol "==" >> Equal fst <$> pNumOp
            ||| do symbol "/=" >> NotEqual fst <$> pNumOp
            ||| do symbol ">=" >> GreaterEqual fst <$> pNumOp
            ||| do symbol ">" >> Greater fst <$> pNumOp
            ||| do symbol "<=" >> LessEqual fst <$> pNumOp
            ||| do symbol "<" >> Less fst <$> pNumOp
            ||| return fst

pExpr :: Parser Expr
pExpr = do symbol "abs" >> Abs <$> pNum -- haskell syntax
         ||| do symbol "if"
                cond <- pBools
                symbol "then"
                true <- pExpr
                symbol "else"
                If cond true <$> pExpr
         ||| pCast
         ||| pArith
              ||| do t <- pTerm
                     do symbol "++" >> Concat t <$> pExpr
                      ||| return t

pNumOp :: Parser Expr
pNumOp = pArith ||| pTerm

pCast :: Parser Expr
pCast = do symbol "toInt("
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

pArith :: Parser Expr
pArith = do t <- pTerm
            do symbol "+" >> Add t <$> pExpr
             ||| do symbol "-" >> Sub t <$> pExpr
             ||| do symbol "^" >> Pow t <$> pExpr
             ||| do symbol "mod" >> Mod t <$> pExpr

pFactor :: Parser Expr
pFactor = pNum ||| pVar ||| pStr ||| pUrgent pExpr

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

pUrgent :: Parser p -> Parser p
pUrgent p = do symbol "("
               e <- p
               symbol ")"
               return e --- expression with priority

pInt :: Parser Expr
pInt = do symbol "(" *> symbol "-"
          d <- many1 digit
          symbol ")"
          return (Val $ NumVal $ Int $ negate $ toInt d) -- negative integer
        ||| do d <- many1 digit
               space
               return (Val $ NumVal $ Int $ toInt d) -- positive integer

pFloat :: Parser Expr
pFloat = do symbol "(" *> symbol "-"
            d <- many1 digit
            symbol "."
            f <- many1 digit
            symbol ")"
            return (Val $ NumVal $ Float $ negate $ read $ d <> "." <> f) -- negative float
          ||| do d <- many1 digit
                 symbol "."
                 f <- many1 digit
                 space
                 return (Val $ NumVal $ Float $ read $ d <> "." <> f) -- positive float

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*" >> Mul f <$> pExpr
            ||| do symbol "/" >> Div f <$> pExpr
            ||| return f

toInt :: String -> Int
toInt = go 0
  where go acc [] = acc
        go acc (x:xs) | 0 <= val && val <= 9 = go (10 * acc + val) xs
                      | otherwise = 0
          where val = digit2Int x

digit2Int :: Char -> Int
digit2Int x = fromEnum x - fromEnum '0'