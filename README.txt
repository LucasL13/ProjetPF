FAUCONNIER Axel
LOIGNON Lucas

I Objectif du projet

II Travail accompli

III Sources
	http://learnyouahaskell.com/chapters
IV Observations



// WORK :

A voir / a faire :
- J'ai rajouté le parsing de commande. Tu peux utiliser la fonction commandParser
			utilisation : commandParser ":set variableA 4+5*(2/4)"
			sortie 			: ["set", "variableA", "4+5*(2/4)"] (tu notera que je retourne pas le ":" devant la commande)
	Et si apres on fait
			-> let x = commandParser ":set variableA 4+5*(2/4)"
			-> evalExpr (last x) ça evalue bien à 6.5

- J'ai rajouté le "-var" / "var" dans la syntaxe. Ca marche nickel d'un point de vue pratique mais j'ai pas encore fais la vraie evaluation.
  Jette un oeil a la ligne 68 de ParserLL. Je pense que c'est un truc comme ça qu'il faut faire mais on verra ensemble si c'est possible


- Demain si de ton coté ça marche pas mal, je rajouterai le "maybe" de partout dans ParserLL
