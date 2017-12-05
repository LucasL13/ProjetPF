module EnvInteractif(mainLoop) where

import Expression
import Parser
import Data.Maybe

--------------------------      Commands           --------------------

-- Déclaration du type Command
data Command = Command {
    name :: String,
    description :: String,
    exits :: Bool,
    run :: Handler
    }

-- Déclaration de la liste des commandes du programme
commands :: [Command]
commands = [
    Command {name = ":quit", description = "Quitte le programme", exits = True, run = handleQuit},
    Command {name = ":help", description = "Affiche les commandes avec des explications", exits = False, run = handleHelp},
    Command {name = ":store", description = "Affiche le contenu du store", exits = False, run = handleStore},
    Command {name = ":set x a", description = "Ajoute la variable x de valeur a au store", exits = False, run = handleSet},
    Command {name = ":unset x", description = "Supprime la variable x du store", exits = False, run = handleUnset}
    ]

-- Construit une chaine de caractère permettant d'afficher la liste des commandes   
commandsToString :: [Command] -> String
commandsToString [] = ""
commandsToString (x:xs) = (name x) ++ "       " ++ (description x) ++ "\n" ++ commandsToString xs

-- Détermine si une chaine de caractère doit être traiter en tant que commande en testant le premier caractère de la chaine
isCommand :: String -> Bool
isCommand (x:xs) = (x == ':')
isCommand [] = False

-- Récupère peut-être une commande basé sur un indice de position obtenu avec la fonction chooseCommand
getCommand ::  Int -> Maybe Command
getCommand (-1) = Nothing
getCommand x = Just (commands !! x) -- ici x forcément valide car on suppose x obtenu avc chooseCommand

-- Renvoi l'indice d'une commande dans la liste commands, ou -1 si la commande n'est pas reconnu
-- Vérification "stricte" des arguments, e.g pas d'arguments superflus
chooseCommand :: [String] -> Int 
chooseCommand (":q":[]) = 0
chooseCommand (":quit":[]) = 0
chooseCommand (":help":[]) = 1
chooseCommand (":h":[]) = 1
chooseCommand (":store":[]) = 2
chooseCommand (":set":_:_:[]) = 3
chooseCommand (":unset":_:[]) = 4
chooseCommand (_:_) = -1

-- Découpe la commande en arguments ( utilisation de la fonction words)
getArgs :: String -> [String]
getArgs xs = words xs

-------------------   Handlers     -----------------------------

-- Déclaration du type Handler
type Handler = [String] -> Store -> IO Store

---------------------------------

-- Handler de la commande :quit
handleQuit :: Handler
handleQuit _ s = return s

---------------------------

-- Handler de la commande :store
handleStore :: Handler
handleStore _ s = do
    putStr ("Contenu du store courant :\n\n" ++ storeToString s)
    return s
    
------------------------------
    
-- Handler de la commande :help
handleHelp :: Handler
handleHelp _ s = do
    putStr(commandsToString commands)
    return s
    
----------------------------------------------------

-- Handler de la commande :set
handleSet :: Handler
handleSet (_:x:y:_) xs = return (addVar x (read y) xs)
handleSet _ xs = return xs

-----------------------------------------------------                     

-- Handler de la commande :unset
handleUnset :: Handler
handleUnset (_:x:[]) s = return (deleteVar x s)
handleUnset _ s = return s

-- Boucle principale

mainLoop :: Store -> IO ()
mainLoop ss = do
  putStr "> "
  xs <- getLine
  if(xs == "") then mainLoop ss else do
    let testCommand = isCommand xs
    if(testCommand == True) then
      do
      let args = getArgs xs
      let mcmd = getCommand (chooseCommand (args))
      if (isJust mcmd) then          
        let cmd = fromJust mcmd in -- ici cmd est une commande valide
          do
          storeTest <- run cmd args ss
          if((exits cmd) == False) then mainLoop storeTest else
            return ()
      else
        do
        putStrLn ("Commande non reconnu, tapez :help ou :h pour obtenir la liste des commandes")
        mainLoop ss
    else do
      let mexpr = parseExpression xs in
        if(isJust mexpr) then
          let mres = eval ss (fromJust mexpr) in
            if(isJust mres) then putStrLn(show (fromJust mres)) else putStrLn("Error, cannot evaluate expression, :store to check the store")
        else putStrLn("Error syntax") 
      mainLoop ss      
      return ()    


-- Vérification string message error -- Pas utilisé/Inutilisable
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
