module ParserLL(ExprType(..), mangeEspace, mangeEspaceFonc, evalExpr, expr, exprBis, term, termBis, unit) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Either

--    E → T E’
--    E’→ -T E' | +T E’ | ε

--    T → U T’
--    T’→ *U T’ | /U T' | ε

--    U → - id | - (E) | (E) | id


-- Parse l'entrée pour match le pattern : 
-- ":command variable value"
-- Retourné sous forme d'un tableau de String [command, variable, value]
commandParserFunc :: Parser [String]
commandParserFunc =
    do 
        char ':'
        a <- letter
        b <- many letter
        char ' '
        c <- letter
        d <- many letter
        char ' '
        e <- anyChar
        f <- many anyChar
        return ([[a]++b, [c]++d, [e]++f])  

-- Fonction pour appeller la fonction précedente avec une simple string à parser
commandParser str =  head (rights ((parse commandParserFunc "" str):[])) 


mangeEspaceFonc :: Parser String
mangeEspaceFonc =   do
		spaces
		sepEndBy (anyChar) spaces

mangeEspace str = head (rights ((parse mangeEspaceFonc "" str):[])) 

-- Permet d'evaluer une String en entrée (str) si cette derniere est conforme à la syntaxe 
evalExpr str =  eval (head (rights ((parse expr "" (mangeEspace str)):[])))


data ExprType = Source ExprType ExprType |
                Add ExprType ExprType | 
                Minus ExprType ExprType | 
                Mult ExprType ExprType | 
                Div ExprType ExprType | 
                Term ExprType | 
                Unit ExprType |
                Negative ExprType |
                Number Double deriving Show

eval (Add a b) = eval a + eval b
eval (Minus a b) = eval a - eval b
eval (Term a) = eval a
eval (Number a) = a
eval (Unit a) = eval a
eval (Mult a b) = (eval a) * (eval b)
eval (Div a b ) = (eval a) / (eval b)
eval (Negative a) = -1 * (eval a)
                


expr :: Parser ExprType
expr = do
            t <- term 
            e <- exprBis t
            return e


exprBis :: ExprType -> Parser ExprType
exprBis s = try
    (
    do
        char '-'
        t <- term
        e <- exprBis (Minus s t)
        return e
    )
    <|>
    try(
    do
        char '+'
        t <- term
        e <- exprBis (Add s t)
        return e
    )
    <|>
    try(
    do
        return s
    )


term :: Parser ExprType
term = do
    u <- unit 
    t <- termBis u 
    return t


termBis :: ExprType -> Parser ExprType
termBis v = try
    (
    do
        char '*'
        u <- unit
        t <- termBis (Mult v u)
        return t
    )
    <|>
    try(
    do
        char '/'
        u <- unit
        t <- termBis (Div v u)
        return t
    )
    <|>
    try(
    do
        return v
    )

unit :: Parser ExprType
unit = try
    (
    do
        char '-'
        x <- digit
        y <- many digit
        char '.'
        x2 <- digit
        y2 <- many digit
        return  (Negative (Number (read ((x:y)++"."++(x2:y2))::Double)))
    )
    <|>
    try(
        do
            char '-'
            x <- digit
            y <- many digit
            return  (Negative (Number (read (x:y)::Double)))
    )
    <|>
    try(
    do
        char '-'
        char '('
        e <- expr
        char ')'
        return (Negative e)
    )
    <|>
    try(
    do
        x <- digit
        y <- many digit
        char '.'
        x2 <- digit
        y2 <- many digit
        return (Number (read ((x:y)++"."++(x2:y2))::Double))
    )
    <|>
    try(
        do
            x <- digit
            y <- many digit
            return (Number (read (x:y)::Double))
    )
    <|>
    try(
    do
        char '('
        e <- expr
        char ')'
        return e
    )