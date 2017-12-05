
import EnvInteractif
import Test.HUnit

{-
isCommand :: String -> Bool
isCommand (x:xs) = (x == ':')
isCommand [] = False
-}
-- Ici le pattern matching est exhaustif, on traite une chaine vide ou une chaine non vide

{-
chooseCommand :: [String] -> Int 
chooseCommand (":q":[]) = 0
chooseCommand (":quit":[]) = 0
chooseCommand (":help":[]) = 1
chooseCommand (":h":[]) = 1
chooseCommand (":store":[]) = 2
chooseCommand (":set":_:_:[]) = 3
chooseCommand (":unset":_:[]) = 4
chooseCommand (_:_) = -1
-}
-- L'utilisation du caractère '_' amène à un pattern matching exhaustif

{-
getCommand ::  Int -> Maybe Command
getCommand (-1) = Nothing
getCommand x = Just (commands !! x)
-}

-- La fonction getCommand peut amener une erreur d'indiçage, mais en pratique l'indice est choisi avec la fonction chooseCommand donc on tombe toujours dans un cas valide


-- Les Handlers de commandes reposent, soit sur des fonctions prouver valide ici, soit sur des fonctions du module Expression qui ont été testées 
