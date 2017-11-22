import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Either


--    E → T E’
--    E’→ -T E' | +T E’ | ε

--    T → U T’
--    T’→ *U T’ | /U T' | ε

--    U → - id | - (E) | (E) | id

mangeEspaceFonc :: Parser String
mangeEspaceFonc = 
	do
		spaces
		sepEndBy (anyChar) spaces

mangeEspace str = head (rights ((parse mangeEspaceFonc "" str):[])) 

test str =  eval (head (rights ((parse expr "" (mangeEspace str)):[])))


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
        e <- exprBis t
        return (Minus s t)
    )
    <|>
    try(
    do
        char '+'
        t <- term
        e <- exprBis t
        return (Add s e)
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
        t <- termBis u
        return (Mult v t)
    )
    <|>
    try(
    do
        char '/'
        u <- unit
        t <- termBis u
        return (Div v t)
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