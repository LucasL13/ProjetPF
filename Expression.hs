-- Fichier decrivant les expressions et leurs évaluations

-- Déclaration du type Variable
-- Une Variable est l'association d'une chaine de caractère et d'une valeur ( ici un Double )
data Variable = Variable {
    varName :: String,
    value :: Double
    }  

-- Déclaration du type Store 
type Store = [Variable]

-- Ajouter type Expression + fonction eval
