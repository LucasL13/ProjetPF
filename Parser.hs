module Parser(parseExpression) where

import Expression
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Either

--    E → T E’
--    E’→ -T E' | +T E’ | ε

--    T → U T’
--    T’→ *U T’ | /U T' | ε

--    U → - id | - var | - (E) | (E) | id | var


mangeEspaceFonc :: Parser String
mangeEspaceFonc = do
  spaces
  sepEndBy (anyChar) spaces

mangeEspace str = head (rights ((parse mangeEspaceFonc "" str):[])) 


-- Permet d'evaluer une String en entrée (xs) en Expression, si cette derniere est conforme à la syntaxe
parseExpression :: String -> Maybe Expression
parseExpression xs = let mexpr = (parse expr "" (mangeEspace xs)) in
                     if isLeft mexpr then Nothing else Just (head (rights (mexpr:[])))

-- Pas d'analyse lexicale
--data Token = IdVar String | -- "xs"
--             Nombre Double | -- x
--             ParentheseOuvrante Char | -- ( 
--            ParentheseFermante Char | -- )
--             SymPlus Char | -- +
--             SymNeg Char | -- -
--             SymMult Char | -- *
--             SymExp Char | -- ^
--             SymDiv Char -- / -- partie II


expr :: Parser Expression
expr = do
            t <- term 
            e <- exprBis t
            return e


exprBis :: Expression -> Parser Expression
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


term :: Parser Expression
term = do
    u <- unit 
    t <- termBis u 
    return t


termBis :: Expression -> Parser Expression
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
        char '^'
        u <- unit
        t <- termBis (Exp v u)
        return t
    )
    <|>
    try(
    do
        return v
    )

unit :: Parser Expression
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
            x <- letter
            y <- many letter
            return  (Negative (IdVar ([x]++y)))
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
            x <- letter
            y <- many letter
            return (IdVar ([x]++y))
    )
    <|>
    try(
    do
        char '('
        e <- expr
        char ')'
        return e
    )
