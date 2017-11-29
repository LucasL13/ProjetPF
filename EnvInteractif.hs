--data StoreElement = MyInt (String,Int) | MyFloat (String, Float)  deriving (Show)
data StoreElement = StoreElement {
    var :: String,
    value :: Float
    }
    
type Store = [StoreElement]

store :: Store
--store = [(MyInt("var1",1)), (MyInt("var2",-6)), (MyFloat("var3",1.5))]
store = [StoreElement {var = "var1", value = 1}, StoreElement {var = "var2", value = -6}, StoreElement {var = "var3", value = 1.5}]
--Handlers 
type Handler = [String] -> Store -> IO Store

handleQuit :: Handler
handleQuit _ s = return s

---------------------------

storeToString :: Store -> String
--storeToString ((MyInt(s,n)):xs) = (s ++ " = " ++ (show n) ++ "\n") ++ storeToString xs
--storeToString ((MyFloat(s,n)):xs) = (s ++ " = " ++ (show n)) ++ storeToString xs
--storeToString [] = "\n"
storeToString (x:xs) = ((var x) ++ " = " ++ (show (value x)) ++ "\n") ++ storeToString xs
storeToString [] = "\n"

handleStore :: Handler
handleStore _ s = do
    putStr (storeToString s)
    return s
    
------------------------------
    
commandsToString :: [Command] -> String
commandsToString [] = "\n"
commandsToString (x:xs) = (name x) ++ "       " ++ (description x) ++ "\n" ++ commandsToString xs
    
handleHelp :: Handler
handleHelp _ s = do
    putStr(commandsToString commands)
    return s
    
---------------

isPresent :: Store -> Bool
isPresent xs = True

--Doit savoir si la variable est un float ou un int ++ check la présence
handleSet :: Handler
handleSet _ xs = return xs

-----------------------------------------------------                     

deleteVar :: String -> Store -> Store
--deleteVar v ((MyInt(s,n)):xs) = if (v == s) then (deleteVar v xs) else ((MyInt(s,n)):(deleteVar v xs))
--deleteVar v ((MyFloat(s,n)):xs) = if (v == s) then (deleteVar v xs) else ((MyFloat(s,n)):(deleteVar v xs))
--deleteVar _ [] = [] 
deleteVar v (x:xs) = if (v == (var x)) then (deleteVar v xs) else (x:(deleteVar v xs))
deleteVar _ [] = []

handleUnset :: Handler
handleUnset (_:x:[]) s = return (deleteVar x s)
handleUnset _ s = return s


--Commands
data Command = Command {
    name :: String,
    description :: String,
    exits :: Bool,
    run :: Handler
    } --deriving (Show)
    
-- q ou quit : on sort du programme
-- h ou help : on affiche la liste des commandes existants avec des explications
-- store : on affiche le contenu du store
-- set x a , ou a est un nombre : ajoute la variable x, avec la valeur a au store
-- unset x : on enleve x du store courant
 
commands :: [Command]
commands = [
    Command {name = ":quit", description = "Quitte le programme", exits = True, run = handleQuit},
    Command {name = ":help", description = "Affiche les commandes avec des explications", exits = False, run = handleHelp},
    Command {name = ":store", description = "Affiche le contenu du store", exits = False, run = handleStore},
    Command {name = ":set x a", description = "Ajoute la variable x de valeur a au store", exits = False, run = handleSet},
    Command {name = ":unset x", description = "Supprime la variable x du store", exits = False, run = handleUnset}
    ]

isCommand :: String -> Bool
isCommand (x:xs) = (x == ':')
isCommand [] = False

main = do
    putStrLn "Hello, what's your name"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock")


mainLoop :: Bool -> IO ()
mainLoop x = if x == True then
    do
        putStr "> "
        xs <- getLine
        putStrLn (xs)
        mainLoop x
    else
        return ()
    
