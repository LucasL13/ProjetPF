import ParserLL

--data StoreElement = MyInt (String,Int) | MyFloat (String, Float)  deriving (Show)
data StoreElement = StoreElement {
    var :: String,
    value :: Double
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
--deleteVar v ((MyFloat(s,n)):xs) = if (v == s) then (deleteVar v xs) else ((MyFloat (s,n)):(deleteVar v xs))
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

faireCommande xs = do
    putStrLn("salut")

parseCommand xs = do
    putStrLn("lel")

faireExpr xs = do
    putStrLn( "> \ESC[34m\STX" ++ xs ++ " = '\ESC[37m\STX' " ++ show (test (xs++""))   )

mainLoop :: IO ()
mainLoop = 
    do
        putStr "> "
        xs <- getLine
        let cmd = isCommand xs
        if(cmd == True) then faireCommande(parseCommand xs)
        else do
            faireExpr xs
            mainLoop        
        return ()
        
-- let args = parseCommand


   

-- getCommand :: Int -> Maybe Command
-- getCommand -1 = Nothing
-- getCommand x = Just (commands !! x)
--
-- stringify :: Maybe Command -> String
-- stringify (Just x) = description x
-- stringify Nothing = "commande inconnu, tapez :help ou :h pour obtenir la liste des commandes"

--chooseCommand au lieu de testPat
testPat :: String -> Int 
testPat ":q" = 0
testPat ":quit" = 0
testPat ":help" = 1
testPat ":h" = 1
testPat ":store" = 2
testPat ":set" = 3
testPat "unset" = 4
testPat _ = -1
