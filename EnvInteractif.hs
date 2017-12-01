import ParserLL
import Data.Maybe


-- Type Store => Déplacer dans un autre fichier

data StoreElement = StoreElement {
    var :: String,
    value :: Double
    }
    
type Store = [StoreElement]

store :: Store
store = [StoreElement {var = "var1", value = 1}, StoreElement {var = "var2", value = -6}, StoreElement {var = "var3", value = 1.5}]


-------------------   Handlers     -----------------------------

-- Déclaration du type Handler
type Handler = [String] -> Store -> IO Store

---------------------------------

-- Handler de la commande :quit
handleQuit :: Handler
handleQuit _ s = return s

---------------------------

-- Construit une chaine de caractère permettant d'afficher le contenu d'un Store
storeToString :: Store -> String
storeToString (x:xs) = ((var x) ++ " = " ++ (show (value x)) ++ "\n") ++ storeToString xs
storeToString [] = ""

-- Handler de la commande :store
handleStore :: Handler
handleStore _ s = do
    putStr (storeToString s)
    return s
    
------------------------------

-- Construit une chaine de caractère permettant d'afficher la liste des commandes   
commandsToString :: [Command] -> String
commandsToString [] = ""
commandsToString (x:xs) = (name x) ++ "       " ++ (description x) ++ "\n" ++ commandsToString xs
    
-- Handler de la commande :help
handleHelp :: Handler
handleHelp _ s = do
    putStr(commandsToString commands)
    return s
    
---------------
-- Renvoi True si la variable x est présente dans le Store, False sinon
isPresent :: String -> Store -> Bool
isPresent _ [] = False
isPresent x (y:ys) = if (x == (var y)) then True else (isPresent x ys)

-- Ajoute la variable x de valeur y au Store zs, si la variable était déjà présente sa valeur sera mise à jour
-- x :: String, y :: Double, zs :: Store
addVar :: String -> Double -> Store -> Store
addVar x y zs = if (isPresent x zs) then ((StoreElement { var = x, value = y}):(deleteVar x zs)) else ((StoreElement { var = x, value = y}):zs)

-- Handler de la commande :set
handleSet :: Handler
handleSet (_:x:y:_) xs = return (addVar x (read y) xs)
handleSet _ xs = return xs

-----------------------------------------------------                     

-- Supprime la variable v du Store
deleteVar :: String -> Store -> Store
deleteVar v (x:xs) = if (v == (var x)) then (deleteVar v xs) else (x:(deleteVar v xs))
deleteVar _ [] = []

-- Handler de la commande :unset
handleUnset :: Handler
handleUnset (_:x:[]) s = return (deleteVar x s)
handleUnset _ s = return s


--------------------------      Commands           --------------------

-- Déclaration du type Command
data Command = Command {
    name :: String,
    description :: String,
    exits :: Bool,
    run :: Handler
    }
    
-- q ou quit : on sort du programme
-- h ou help : on affiche la liste des commandes existants avec des explications
-- store : on affiche le contenu du store
-- set x a , ou a est un nombre : ajoute la variable x, avec la valeur a au store
-- unset x : on enleve x du store courant

-- Déclaration de la liste des commandes du programme
commands :: [Command]
commands = [
    Command {name = ":quit", description = "Quitte le programme", exits = True, run = handleQuit},
    Command {name = ":help", description = "Affiche les commandes avec des explications", exits = False, run = handleHelp},
    Command {name = ":store", description = "Affiche le contenu du store", exits = False, run = handleStore},
    Command {name = ":set x a", description = "Ajoute la variable x de valeur a au store", exits = False, run = handleSet},
    Command {name = ":unset x", description = "Supprime la variable x du store", exits = False, run = handleUnset}
    ]

-- Détermine si une chaine de caractère doit être traiter en tant que commande en testant le premier caractère de la chaine
isCommand :: String -> Bool
isCommand (x:xs) = (x == ':')
isCommand [] = False

faireCommande xs = do
    putStrLn("salut")

parseCommand :: String -> [String]
parseCommand x = [x]
parseCommand _ = []
--parseCommand xs = do
   -- putStrLn("lel")

faireExpr xs = do
    putStrLn( "> \ESC[34m\STX" ++ xs ++ " = '\ESC[37m\STX' " ++ show (test (xs++""))   )

-- afficher "type :help to see the list of commands"


storeMainLoop :: Store -> IO ()
storeMainLoop ss = 
    do
        putStr "> "
        xs <- getLine
        let testCommand = isCommand xs
        if(testCommand == True) then
            do
            let args = parseCommand xs
            let mcmd = checkCommand (chooseCommand (args))
            if (isJust mcmd) then          
                let cmd = fromJust mcmd in -- ici cmd est une commande valide
                    do
                    storeTest <- run cmd args ss
                    if((exits cmd) == False) then storeMainLoop storeTest else
                        return ()
            else
                do
                putStrLn ("Commande non reconnu, tapez :help ou :h pour obtenir la liste des commandes")
                storeMainLoop ss
        else do
            putStrLn xs
            storeMainLoop  ss      
        return ()

-- Lance la boucle principale avec un store vide
main = mainLoop []

mainLoop :: Store -> IO ()
mainLoop ss = 
    do
        putStr "> "
        xs <- getLine
        let testCommand = isCommand xs
        if(testCommand == True) then
            do
            let args = parseCommand xs
            let mcmd = checkCommand (chooseCommand (args))
            if (isJust mcmd) then          
                let cmd = fromJust mcmd in -- ici cmd est une commande valide
                    do
                    storeTest <- run cmd args []
                    if((exits cmd) == False) then mainLoop storeTest else
                        return ()
            else
                do
                putStrLn ("Commande non reconnu, tapez :help ou :h pour obtenir la liste des commandes")
                mainLoop ss
        else do
            putStrLn xs
            mainLoop ss      
        return ()
        
-- let args = parseCommand

-- Vérifie si la commande est valide
checkCommand :: Int -> Maybe Command
checkCommand (-1) = Nothing
checkCommand x = Just (commands !! x)

-- Récupère peut-être une commande basé sur un indice de position
--getCommand ::  Int -> Maybe Command
--getCommand -1 = Nothing
--getCommand x = Just (commands !! x)

stringify :: Maybe Command -> String
stringify (Just x) = description x
stringify Nothing = "commande inconnu, tapez :help ou :h pour obtenir la liste des commandes"

-- Renvoi l'indice d'une commande dans la liste commands, ou -1 si la commande n'est pas reconnu
-- Pas de vérification
chooseCommand :: [String] -> Int 
chooseCommand (":q":_) = 0
chooseCommand (":quit":_) = 0
chooseCommand (":help":_) = 1
chooseCommand (":h":_) = 1
chooseCommand (":store":_) = 2
chooseCommand (":set":_) = 3
chooseCommand (":unset":_) = 4
chooseCommand (_:_) = -1

-- Vérification string message error
verifyCommand :: [String] -> Maybe String
verifyCommand (":q":[]) = Nothing
verifyCommand (":q":xs) = Just "Trop d'arguments"
verifyCommand (":quit":[]) = Nothing
verifyCommand (":quit":xs) = Just "Trop d'arguments"
verifyCommand (":help":[]) = Nothing
verifyCommand (":help":xs) = Just "Trop d'arguments"
verifyCommand (":h":[]) = Nothing
verifyCommand (":h":xs) = Just "Trop d'arguments"
verifyCommand (":store":[]) = Nothing
verifyCommand (":store":xs) = Just "Trop d'arguments"
verifyCommand (":set":v:x:[]) = Nothing
verifyCommand (":set":v:x:xs) = Just "Trop d'arguments"
verifyCommand (":set":xs) = Just "Pas assez d'arguments"
verifyCommand (":unset":[]) = Nothing
verifyCommand (":unset":xs) = Just "Trop d'arguments"
verifyCommand (_:_) = Just "commande inconnu, tapez :help ou :h pour obtenir la liste des commandes"
