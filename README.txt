FAUCONNIER Axel
LOIGNON Lucas

I Objectif du projet

	L'objectif de ce projet est d'implémenter un mini environnement interactif, à la manière de ghci, permettant dans un premier temps d'évaluer des expressions numériques simples.
Pour cela nous avons implémenté :
 - le module Parser, dont la fonction parseExpression effectue une analyse syntaxique d'une chaine de caractère et qui construit un objet de type Maybe Expression
 - le module Expression qui fournit la fonction 'eval' qui prends un objet de type Expression et renvoie Maybe Double
 - le module EnvInteractif qui utilise les fonctions principales des deux modules précédents pour créer l'environnement interactif
 
II Travail accompli

	Partie I :
		Parser.hs				|	90% fini, léger problème dans l'analyse syntaxique
		Expression.hs			|	100% fini
		EnvInteractif.hs		|	100% fini
		ParserTest.hs			|	Tests unitaires (27)
		ExpressionTest.hs		|	Test unitaires (15)
		EnvInteractifTest.hs	|	Pas de test unitaires, validité des fonctions prouvée
		
	Partie II :
		Ajout de l'opérateur binaire de division '/'

III Sources

	Ressources disponibles sur internet qui nous ont été utiles pour ce projet : 

		* http://learnyouahaskell.com/chapters  		Utile en complément du cours
		* https://en.wikipedia.org/wiki/Left_recursion 	Aide pour former une grammaire et l'analyse syntaxique
		* https://hackage.haskell.org/ 					Pour la documentation
	
IV Observations
