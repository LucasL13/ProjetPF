
import Parser
import Expression
import Test.HUnit

test1 = TestCase (assertEqual "chaine vide" Nothing (parseExpression "") )

test2 = TestCase (assertEqual "3" (Just (Number 3)) (parseExpression "3"))

test3 = TestCase (assertEqual "-3" (Just (Negative (Number 3))) (parseExpression "-3"))

test4 = TestCase (assertEqual "3 + 2" (Just (Add (Number 3) (Number 2))) (parseExpression "3+2"))

test5 = TestCase (assertEqual "3 * 2" (Just (Mult (Number 3) (Number 2))) (parseExpression "3*2"))

test6 = TestCase (assertEqual "3 ^ 2" (Just (Exp (Number 3) (Number 2))) (parseExpression "3^2"))

test7 = TestCase (assertEqual "3 + 2 * 5" (Just (Add (Number 3) (Mult (Number 2) (Number 5)))) (parseExpression "3+2*5"))

test8 = TestCase (assertEqual "5*3 + 2" (Just (Add (Mult (Number 5) (Number 3)) (Number 2))) (parseExpression "5*3+2"))

test9 = TestCase (assertEqual "3 + x" (Just (Add (Number 3) (IdVar "x"))) (parseExpression "3 + x"))

test10 = TestCase (assertEqual "3 + " Nothing (parseExpression "3 + "))

test11 = TestCase (assertEqual "+" Nothing (parseExpression "+"))

test12 = TestCase (assertEqual "-" Nothing (parseExpression "-"))

test13 = TestCase (assertEqual "*" Nothing (parseExpression "*"))

test14 = TestCase (assertEqual "^" Nothing (parseExpression "^"))

test15 = TestCase (assertEqual "3 - " Nothing (parseExpression "3 - "))

test16 = TestCase (assertEqual "3 * " Nothing (parseExpression "3 * "))

test17 = TestCase (assertEqual "3 ^ " Nothing (parseExpression "3 ^ "))

test18 = TestCase (assertEqual "+3" Nothing (parseExpression "+3"))

test19 = TestCase (assertEqual "*3" Nothing (parseExpression "*3"))

test20 = TestCase (assertEqual "^3" Nothing (parseExpression "^3"))

test21 = TestCase (assertEqual "3.21" (Just (Number 3.21)) (parseExpression "3.21"))

test22 = TestCase (assertEqual "-3.21" (Just (Negative (Number 3.21))) (parseExpression "-3.21"))

test23 = TestCase (assertEqual ".21" Nothing (parseExpression ".21"))

test24 = TestCase (assertEqual "-.21" Nothing (parseExpression "-.21"))

test25 = TestCase (assertEqual "()" Nothing (parseExpression "()"))

test26 = TestCase (assertEqual "(3 + 2)" (Just (Add (Number 3) (Number 2))) (parseExpression "(3 +2)"))

test27 = TestCase (assertEqual "(3 + 2) * 2" (Just (Mult (Add (Number 3) (Number 2)) (Number 2))) (parseExpression "(3 +2) * 2"))

--(Just (Add (Number 3) (Number 2)))
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
                  TestLabel "test15" test15,
                  TestLabel "test16" test16,
                  TestLabel "test17" test17,
                  TestLabel "test18" test18,
                  TestLabel "test19" test19,
                  TestLabel "test20" test20,
                  TestLabel "test21" test21,
                  TestLabel "test22" test22,
                  TestLabel "test23" test23,
                  TestLabel "test24" test24,
                  TestLabel "test25" test25,
                  TestLabel "test26" test26,
                  TestLabel "test27" test27]
