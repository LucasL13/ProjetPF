
import Expression
import Test.HUnit

store :: Store
store = [Variable { varName = "x", value = 1}]

-- Test fonction eval

-- Test : évaluation Add correcte
test1 = TestCase (assertEqual "3 + 1" (Just 4) (eval store (Add (Number 3) (Number 1))))
-- Test : échec de l'évaluation si variable inconnu
test11 = TestCase (assertEqual "3 + y" Nothing (eval store (Add (Number 3) (IdVar "y"))))
-- Test : évaluation correcte avec variable connu
test12 = TestCase (assertEqual "3 + x" (Just 4) (eval store (Add (Number 3) (IdVar "x"))))
-- Test : évaluation Mult correcte
test13 = TestCase (assertEqual "3 * 2" (Just 6) (eval store (Mult (Number 3) (Number 2))))
-- Test : évaluation Negative correcte
test14 = TestCase (assertEqual "-3" (Just (-3)) (eval store (Negative (Number 3))))
-- Test : évaluation Exp correcte
test15 = TestCase (assertEqual "3 ^ 2" (Just 9) (eval store (Exp (Number 3) (Number 2))))


-- Test fonctions liés au Store

-- Test : la variable est bien présente
test2 = TestCase (assertEqual "isPresent x" True (isPresent "x" store))
-- Test : la variable est bien absente
test3 = TestCase (assertEqual "isPresent y" False (isPresent "y" store))

--getValue :: String -> Store -> Maybe Double
-- Test : on obtient bien la valeur de la variable
test4 = TestCase (assertEqual "getValue x" (Just 1) (getValue "x" store))
-- Test : on n'obtient pas de valeur pour une variable inconnu
test5 = TestCase (assertEqual "getValue y" Nothing (getValue "y" store))

--deleteVar :: String -> Store -> Store
-- Test : la variable est bien supprimé
test6 = TestCase (let nstore = deleteVar "x" store in
                  assertEqual "deleteVar x" False (isPresent "x" nstore))
-- Test : on ne supprime pas une autre variable
test7 = TestCase (let nstore = deleteVar "y" store in
                  assertEqual "deleteVar y" True (isPresent "x" nstore))

--addVar :: String -> Double -> Store -> Store
-- Test : la variable est bien ajouté
test8 = TestCase (let nstore = addVar "y" 5 store in
                  assertEqual "addVar y" True (isPresent "y" nstore))
-- Test : On peut récuperer la valeur de la variable ajouté
test9 = TestCase (let nstore = addVar "y" 5 store in
                  assertEqual "getvalue - addVar y" (Just 5) (getValue "y" nstore))
-- Test : On vérifie que l'ajout d'une variable déjà existante modifie sa valeur
test10 = TestCase (let nstore = addVar "x" 5 store in
                  assertEqual "getvalue - addVar y" (Just 5) (getValue "x" nstore))

tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6,
                  TestLabel "test7" test7,
                  TestLabel "test8" test8,
                  TestLabel "test9" test9,
                  TestLabel "test10" test10,
                  TestLabel "test11" test11,
                  TestLabel "test12" test12,
                  TestLabel "test13" test13,
                  TestLabel "test14" test14,
                  TestLabel "test15" test15]
