module Expr_parsing where

import Parsing
import Data_type

pBatch :: Parser [Command]
pBatch = do many pComment -- comment before/ after command
            do fst <- many1 (space *> pCommand <* space)
               rest <- pBatch
               return (fst ++ rest)
             ||| return [] -- empty loop/ function/ file

--Parses comments denoted by "--"
pComment :: Parser ()
pComment = symbol "--" *> many (sat (/= '\n')) *> space -- ignoring comments

--Parses a command from the user
pCommand :: Parser Command
pCommand = pFunc ||| pCond ||| pSet
            ||| do Read <$> (symbol "read" *> many (sat (/= '\"'))) -- '"' is not supported
            ||| do Print <$> (symbol "print" *> pExpr)
            ||| do symbol "quit" >> return Quit


--Parses a function
pFunc :: Parser Command
pFunc = do name <- symbol "void" *> many1 (letter ||| char '_') -- setting function
           argv <- symbol "(" *> pHead (many1 (letter ||| char '_')) <* symbol ")" -- arguments
           SetFunc name argv <$> pBody -- body
         ||| do name <- many1 (letter ||| char '_') -- applying function
                argv <- symbol "(" *> pHead pExpr <* symbol ")" -- arguments
                return $ Func name argv

--Parses a "set" command (i.e. x = 5)
pSet :: Parser Command
pSet = do t <- many1 (letter ||| char '_') -- variable name
          Set t <$> (symbol "=" *> pExpr) -- value

--Parses a conditional statement (if then else, while, repeat, do, for, etc)
pCond :: Parser Command
pCond = do cond <- symbol "if" *> symbol "(" *> pBools <* symbol ")" -- non-empty list of boolean expressions
           true <- pBody -- body of 'then' part
           do Cond cond true <$> (symbol "else" *> pBody) -- body of 'else' part
            ||| return (Cond cond true []) -- there can be no 'else' part
         ||| do acc <- symbol "repeat" *> many1 digit -- only integer is accepted
                Repeat (toInt acc) <$> pBody
         ||| do cond <- symbol "while" *> symbol "(" *> pBools <* symbol ")" -- non-empty list of boolean expressions
                While cond <$> pBody
         ||| do cmd <- symbol "do" *> pBody
                cond <- symbol "while" *> symbol "(" *> pBools <* symbol ")" -- non-empty list of boolean expressions
                return $ DoWhile cond cmd
         ||| do init <- symbol "for" *> symbol "(" *> pHead pSet <* symbol ";" -- initialization
                cond <- pBools <* symbol ";" -- non-empty list of boolean expressions
                after <- pHead pSet <* symbol ")" -- afterthought
                For init cond after <$> pBody

pHead :: Parser a -> Parser [a]
pHead p = do fst <- p
             rest <- many (symbol "," *> p)
             return (fst:rest) -- get a generic list
           ||| return [] -- empty list

--Parses the body of a conditional or anything that uses "xyz(abc) { def }"" notation
pBody :: Parser [Command]
pBody = do symbol "{" *> many pComment
           do fst <- pCommand
              rest <- many (many (symbol ";") *> many pComment *> pCommand)
              many pComment *> symbol "}"
              return (fst:rest)
            ||| do symbol "}" >> return [] -- empty body

--Parses the booleans in a conditional (if (xyz <-- this part))
pBools :: Parser Expr
pBools = do Not <$> (symbol "!" *> symbol "(" *> (pBools ||| pUrgent pBools) <* symbol ")") -- NOT operation
          ||| do fst <- pBool ||| pUrgent pBools
                 do And fst <$> (symbol "&&" *> pBools) -- AND operation
                  ||| do Or fst <$> (symbol "||" *> pBools) -- OR operation
                  ||| return fst -- accept one or more clauses

--Parses an individual Boolean in a conditional (i.e. x==5)
pBool :: Parser Expr
pBool = do fst <- pNumOp
           do Equal fst <$> (symbol "==" *> pNumOp)
            ||| do NotEqual fst <$> (symbol "/=" *> pNumOp)
            ||| do GreaterEqual fst <$> (symbol ">=" *> pNumOp)
            ||| do Greater fst <$> (symbol ">" *> pNumOp)
            ||| do LessEqual fst <$> (symbol "<=" *> pNumOp)
            ||| do Less fst <$> (symbol "<" *> pNumOp)

--Parses an expression
pExpr :: Parser Expr
pExpr = do Abs <$> (symbol "abs" *> pNumOp ||| pVar) -- accetping only variable names & arithmetic expression
         ||| do cond <- symbol "if" *> pBools -- condition
                true <- symbol "then" *> pExpr -- 'true' part
                symbol "else" >> If cond true <$> pExpr -- 'false' part (must not be empty)
         ||| pCast
         ||| pArith
              ||| do t <- pTerm
                     do Concat t <$> (symbol "++" *> pExpr)
                      ||| return t

--Parses a numerical operation and the terms (or just a term), i.e. (5+4)
pNumOp :: Parser Expr
pNumOp = pArith ||| pTerm -- NumOp -> number operation

--Parses a casted expression in toNum
pCast :: Parser Expr
pCast = do symbol "toNum" <* symbol "("
           do ToNum <$> (char '\"' *> pNum <* char '\"' <* symbol ")") -- String to Float/ Int
            ||| do ToNum . Get <$> (many1 (letter ||| char '_') <* symbol ")")-- variable
         ||| do ToString <$> (symbol "toStr" *> symbol "(" *> pExpr <* symbol ")") -- Int/ Float to String

--Parses an arithmetic expression
pArith :: Parser Expr
pArith = do t <- pTerm
            do Add t <$> (symbol "+" *> pExpr)
             ||| do Sub t <$> (symbol "-" *> pExpr)
             ||| do Pow t <$> (symbol "^" *> pExpr)
             ||| do Mod t <$> (symbol "mod" *> pExpr)

--Parses a Number, Variable, String, or Expression surrounded by parentheses
pFactor :: Parser Expr
pFactor = pNum ||| pVar ||| pStr ||| pUrgent pExpr -- basic units

--Parses a number (using pFloat or pInt)
pNum :: Parser Expr
pNum = pFloat ||| pInt -- basic data types

--Parses a variable (i.e. x or num_things)
pVar :: Parser Expr
pVar = do Val . StrVal <$> symbol "input" -- input keyword for user input
        ||| do Get <$> (many1 (letter ||| char '_') <* space) -- variable

--Parses a string (surrounded by double quotes)
pStr :: Parser Expr
pStr = Val . StrVal <$> (char '\"' *> many (sat (/= '\"')) <* char '\"' <* space) -- string (empty string is possible)

--Parses an expression in paremtheses
pUrgent :: Parser p -> Parser p
pUrgent p = symbol "(" *> p <* symbol ")" --- expression with priority

--Parses an Int
pInt :: Parser Expr
pInt = do d <- symbol "(" *> symbol "-" *> many1 digit <* symbol ")"
          return (Val $ NumVal $ Int $ negate $ toInt d) -- negative integer
        ||| do d <- many1 digit <* space
               return (Val $ NumVal $ Int $ toInt d) -- positive integer

--Parses a Float
pFloat :: Parser Expr
pFloat = do d <- symbol "(" *> symbol "-" *> many1 digit
            f <- symbol "." *> many1 digit <* symbol ")"
            return (Val $ NumVal $ Float $ negate $ read $ d <> "." <> f) -- negative float
          ||| do d <- many1 digit <* symbol "."
                 f <- many1 digit <* space
                 return (Val $ NumVal $ Float $ read $ d <> "." <> f) -- positive float

--Parses an individual term
pTerm :: Parser Expr
pTerm = do f <- pFactor
           do Mul f <$> (symbol "*" *> pExpr)
            ||| do Div f <$> (symbol "/" *> pExpr)
            ||| return f

--Converts a String to an Int (returns 0 if the parse failed)
toInt :: String -> Int
toInt = go 0
  where go acc [] = acc
        go acc (x:xs) | 0 <= val && val <= 9 = go (10 * acc + val) xs
                      | otherwise = 0
          where val = digit2Int x

--Converts a Char to an Int (only designed to work with digits)
digit2Int :: Char -> Int
digit2Int x = fromEnum x - fromEnum '0'