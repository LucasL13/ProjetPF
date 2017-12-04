module Expression (Variable(..), Store(..), Expression(..), storeToString, isPresent, getValue, deleteVar, addVar, eval) where

-- Fichier décrivant les expressions et leurs évaluations

import Data.Maybe

-- Déclaration du type Variable
-- Une Variable est l'association d'une chaine de caractère et d'une valeur ( ici un Double )
data Variable = Variable {
    varName :: String,
    value :: Double
    }  

-- Déclaration du type Store 
type Store = [Variable]

-------------------- Fonction de manipulation d'un Store -------------------

-- Construit une chaine de caractère permettant d'afficher le contenu d'un Store
storeToString :: Store -> String
storeToString (x:xs) = ((varName x) ++ " = " ++ (show (value x)) ++ "\n") ++ storeToString xs
storeToString [] = ""

-- Renvoi True si la variable x est présente dans le Store, False sinon
isPresent :: String -> Store -> Bool
isPresent _ [] = False
isPresent x (y:ys) = if (x == (varName y)) then True else (isPresent x ys)

-- Renvoi l'objet Variable dont le champ varName correspond à la chaine de caractère en argument si la variable est présente dans le Store
getVariable :: String -> Store -> Maybe Variable
getVariable _ [] = Nothing
getVariable vn (x:xs) = if (vn == varName x) then Just x else (getVariable vn xs)

-- Récupère le champ value d'une variable de nom v si la variable est présente dans le Store
getValue :: String -> Store -> Maybe Double
getValue v ss = let x = getVariable v ss in
                if(isJust x) then Just (value (fromJust x)) else Nothing

-- Supprime la variable v du Store
deleteVar :: String -> Store -> Store
deleteVar v (x:xs) = if (v == (varName x)) then (deleteVar v xs) else (x:(deleteVar v xs))
deleteVar _ [] = []
                
-- Ajoute la variable x de valeur y au Store zs, si la variable était déjà présente sa valeur sera mise à jour
-- x :: String, y :: Double, zs :: Store
addVar :: String -> Double -> Store -> Store
addVar x y zs = if (isPresent x zs) then ((Variable { varName = x, value = y}):(deleteVar x zs)) else ((Variable { varName = x, value = y}):zs)


-------------------------------------------------------------------------------

-- Déclaration du type Expression
data Expression = Source Expression Expression |
                Add Expression Expression | 
                Minus Expression Expression |
                Mult Expression Expression | 
                Div Expression Expression |  --part 2
                Term Expression | 
                Unit Expression |
                Negative Expression |
                IdVar String |
                Number Double deriving Show

-- Fonction d'évaluation d'une expression, en fonction d'un Store
eval :: Store -> Expression -> Maybe Double
eval ss (Number x) = Just x
eval ss (IdVar x) = getValue x ss
eval ss (Negative x) = let mx = eval ss x in
                       if (isJust mx) then Just (-1 * (fromJust mx)) else Nothing
eval ss (Add x y) = let mx = eval ss x 
                        my = eval ss y in
                    if ((isJust mx) && (isJust my)) then Just ((fromJust mx) + (fromJust my)) else Nothing
eval ss (Minus x y) = let mx = eval ss x 
                          my = eval ss y in
                    if ((isJust mx) && (isJust my)) then Just ((fromJust mx) - (fromJust my)) else Nothing
eval ss (Mult x y) = let mx = eval ss x
                         my = eval ss y in
                    if ((isJust mx) && (isJust my)) then Just ((fromJust mx) * (fromJust my)) else Nothing
--manque exponentiation
                    
