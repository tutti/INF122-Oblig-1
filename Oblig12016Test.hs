module Oblig12016Test where
import Oblig12016
import Test.HUnit

-- Her kan du legge til dine egne tester.

mineEgeneTester = TestList []

-- Følgende tester må kjøre for å få en A.

testKontoAst = TestList [
  TestCase (assertEqual "parseBlock (tokenize \"set konto lambda balanse (lambda diff (set balanse (balanse, diff)+ ));set a konto (100);a (13);a (2);\")"
            (Block [Set "konto" (Lambda "balanse" (Lambda "diff" (Set "balanse" (App (Name "+") [Name "balanse",Name "diff"])))),Set "a" (App (Name "konto") [Number 100]),App (Name "a") [Number 13],App (Name "a") [Number 2]],[]) (parseBlock (tokenize "set konto lambda balanse (lambda diff (set balanse (balanse, diff)+ ));set a konto (100);a (13);a (2);")))]

testKonto = TestList [
  TestCase (assertEqual "run \"set konto lambda balanse (lambda diff (set balanse (balanse, diff)+ ));set a konto (100);a (13);a (2);\""
            (Number 115) (run "set konto lambda balanse (lambda diff (set balanse (balanse, diff)+ ));set a konto (100);a (13);a (2);")),
  TestCase (assertEqual "run \"set a 1; set y lambda x (a); set a 4; y (1);\""
            (Number 4) (run "set a 1; set y lambda x (a); set a 4; y (1);"))]

testCbrActualAstA = TestList [
 TestCase (assertEqual "parseBlock (tokenize \"set v 10; lambda x (set x (x, 1)+) (v); v;\")" (Block [Set "v" (Number 10),App (Lambda "x" (Set "x" (App (Name "+") [Name "x",Number 1]))) [Name "v"],Name "v"],[]) (parseBlock (tokenize "set v 10; lambda x (set x (x, 1)+) (v); v;"))),
 TestCase (assertEqual "parseBlock (tokenize \"set dereference lambda x (x); set v 10; lambda x (set x (x, 1)+) (dereference (v));v;\")" (Block [Set "dereference" (Lambda "x" (Name "x")),Set "v" (Number 10),App (Lambda "x" (Set "x" (App (Name "+") [Name "x",Number 1]))) [App (Name "dereference") [Name "v"]],Name "v"],[]) (parseBlock (tokenize "set dereference lambda x (x); set v 10; lambda x (set x (x, 1)+) (dereference (v));v;")))]
 
testCbrActualA = TestList [
 TestCase (assertEqual "run \"set v 10; lambda x (set x (x, 1)+) (v); v;\"" (Number 11) (run "set v 10; lambda x (set x (x, 1)+) (v); v;")),
 TestCase (assertEqual "run \"set dereference lambda x (x); set v 10; lambda x (set x (x, 1)+) (dereference (v));v;\"" (Number 10) (run "set dereference lambda x (x); set v 10; lambda x (set x (x, 1)+) (dereference (v));v;"))]

-- Følgende tester må kjøre for å få en B.

testCbrActualAstB = TestList [
 TestCase (assertEqual "parseBlock (tokenize \"lambda x (x) (2);\")" (Block [App (Lambda "x" (Name "x")) [Number 2]],[]) (parseBlock (tokenize "lambda x (x) (2);"))),
 TestCase (assertEqual "parseBlock (tokenize \"lambda x (x) ((2,1)+);\")" (Block [App (Lambda "x" (Name "x")) [App (Name "+") [Number 2,Number 1]]],[]) (parseBlock (tokenize "lambda x (x) ((2,1)+);"))),
 TestCase (assertEqual "parseBlock (tokenize \"set v 1; lambda x (x) (lambda x (x) (v));\")" (Block [Set "v" (Number 1),App (Lambda "x" (Name "x")) [App (Lambda "x" (Name "x")) [Name "v"]]],[]) (parseBlock (tokenize "set v 1; lambda x (x) (lambda x (x) (v));")))]

testCbrActualB = TestList [
 TestCase (assertEqual "run \"lambda x (x) (2);\"" (Number 2) (run "lambda x (x) (2);")),
 TestCase (assertEqual "run \"lambda x (x) ((2,1)+);\"" (Number 3) (run "lambda x (x) ((2,1)+);")),
 TestCase (assertEqual "run \"set v 1; lambda x (x) (lambda x (x) (v));\"" (Number 1) (run "set v 1; lambda x (x) (lambda x (x) (v));"))]

-- Følgende tester må kjøre for å få en C.

testCbrActualAstC = TestList [
  TestCase (assertEqual "parse \"(0,1)+;(0,1)-;\""
           (Block [App (Name "+") [Number 0,Number 1],App (Name "-") [Number 0,Number 1]]) (parse "(0,1)+;(0,1)-;")),
  TestCase (assertEqual "parse \"set a (0, 1)-; (a, 3)+;\""
           (Block [Set "a" (App (Name "-") [Number 0,Number 1]),App (Name "+") [Name "a",Number 3]]) (parse "set a (0, 1)-; (a, 3)+;"))]
testCbrActualC = TestList [
  TestCase (assertEqual "run \"(0,1)+;(0,1)-;\") [] [] " (Number (-1)) (run "(0,1)+;(0,1)-;" )),
  TestCase (assertEqual "run \"set a (0, 1)-; (a, 3)+;\" " (Number 2) (run "set a (0, 1)-; (a, 3)+;"))]

-- Følgende tester må kjøre for å få en D.

testCbrActualD = TestList[
 TestCase (assertEqual "run \"case otherwise->(1,1)-.;\"" (Number 0) (run "case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,1)==->1, case otherwise->(1,1)-.;\"" (Number 1) (run "case (1,1)==->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,2)==->1, case otherwise->(1,1)-.;\"" (Number 0) (run "case (1,2)==->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,1)!=->1, case otherwise->(1,1)-.;\"" (Number 0) (run "case (1,1)!=->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,2)!=->1, case otherwise->(1,1)-.;\"" (Number 1) (run "case (1,2)!=->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,0)>->1, case otherwise->(1,1)-.;\"" (Number 1) (run "case (1,0)>->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,2)>->1, case otherwise->(1,1)-.;\"" (Number 0) (run "case (1,2)>->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,1)>->1, case otherwise->(1,1)-.;\"" (Number 0) (run "case (1,1)<->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,2)>->1, case otherwise->(1,1)-.;\"" (Number 1) (run "case (1,2)<->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,1)>->1, case otherwise->(1,1)-.;\"" (Number 0) (run "case (1,1)<->1, case otherwise->(1,1)-.;")),
 TestCase (assertEqual "run \"case (1,1)==->case (2,1)>-> 1, case otherwise-> 2., case (0,1)!=-> 3, case otherwise->4.;\"" (Number 1) (run "case (1,1)==->case (2,1)>-> 1, case otherwise-> 2., case (0,1)!=-> 3, case otherwise->4.;")),
 TestCase (assertEqual "run \"case (1,1)==->case (1,2)>-> 1, case otherwise-> 2., case (0,1)!=-> 3, case otherwise->4.;\"" (Number 2) (run "case (1,1)==->case (1,2)>-> 1, case otherwise-> 2., case (0,1)!=-> 3, case otherwise->4.;")),
 TestCase (assertEqual "run \"case (1,1)!=->case (1,2)>-> 1, case otherwise-> 2., case (0,1)!=-> 3, case otherwise->4.;\"" (Number 3) (run "case (1,1)!=->case (1,2)>-> 1, case otherwise-> 2., case (0,1)!=-> 3, case otherwise->4.;")),
 TestCase (assertEqual "run \"case (1,1)!=->case (1,2)>-> 1, case otherwise-> 2., case (0,0)!=-> 3, case otherwise->4.;\"" (Number 4) (run "case (1,1)!=->case (1,2)>-> 1, case otherwise-> 2., case (0,0)!=-> 3, case otherwise->4.;"))]

testCbrActualAstD = TestList [
  TestCase (assertEqual "parseBlock (tokenize \"case otherwise->(1,1)-.;\")" (Block [Case Default [(App (Name "-") [Number 1,Number 1])]],[]) (parseBlock (tokenize "case otherwise->(1,1)-.;"))),
  
  TestCase (assertEqual
            "parseBlock (tokenize \"case (1,1)==->1, case otherwise->(1,1)-.;\")"
            (Block [Case (Bool (Name "==") (Number 1) (Number 1))
                     [(Number 1),
                      (Case Default [(App (Name "-") [Number 1,Number 1])])]], []) (parseBlock (tokenize "case (1,1)==->1, case otherwise->(1,1)-.;"))),
  
  TestCase (assertEqual
            "parseBlock (tokenize \"case (1,1)!=->1, case otherwise->(1,1)-.;\")"
            (Block [Case (Bool (Name "!=") (Number 1) (Number 1))
                    [(Number 1),
                     (Case Default [App (Name "-") [Number 1,Number 1]] )]],[])
             (parseBlock (tokenize "case (1,1)!=->1, case otherwise->(1,1)-.;"))),
    
  TestCase (assertEqual
            "parseBlock (tokenize \"case (1,1)>->1, case otherwise->(1,1)-.;\")"
            (Block [Case (Bool (Name ">") (Number 1) (Number 1))
                    [(Number 1),
                     (Case Default [(App (Name "-") [Number 1,Number 1])])]],[])
             (parseBlock (tokenize "case (1,1)>->1, case otherwise->(1,1)-.;"))),
    
  TestCase (assertEqual
            "parseBlock (tokenize \"case (1,1)==->case (1,2)>-> 1, case otherwise-> 2., case (0,1)!=-> 3, case otherwise->4.;\")"
            (Block [Case (Bool (Name "==") (Number 1) (Number 1))
                    [(Case (Bool (Name ">") (Number 1) (Number 2))
                      [Number 1,
                      (Case Default [Number 2])]),
                      (Case (Bool (Name "!=") (Number 0) (Number 1))
                       [(Number 3), (Case Default [Number 4])])]],[])
             (parseBlock (tokenize "case (1,1)==->case (1,2)>-> 1, case otherwise-> 2., case (0,1)!=-> 3, case otherwise->4.;")))]

-- Følgende tester må kjøre for å få en E.

testCbrActualE = TestList[
  TestCase (assertEqual "run \"(0,1)+;\")" (Number 1) (run "(0,1)+;")),
  TestCase (assertEqual "run \"((0,1)*, 2) +;\")" (Number 2) (run "((0,1)*, 2) +;")),
  TestCase (assertEqual "run \"((0,1)*, (2, 2)/) +;\")" (Number 1) (run "((0,1)*, (2, 2)/) +;"))]

testCbrActualAstE = TestList [
  TestCase (assertEqual "parseBlock (tokenize \"(0,1)+;\")" (Block [App (Name "+") [Number 0,Number 1]], []) (parseBlock (tokenize "(0,1)+;"))),
  TestCase (assertEqual "parseBlock (tokenize \"((0,1)*, 2) +;\")" (Block [App (Name "+") [App (Name "*") [Number 0,Number 1],Number 2]],[]) (parseBlock (tokenize "((0,1)*, 2) +;"))),
  TestCase (assertEqual "parseBlock (tokenize \"((0,1)*, (2, 2)/) +;\")" (Block [App (Name "+") [App (Name "*") [Number 0,Number 1],App (Name "/") [Number 2,Number 2]]],[]) (parseBlock (tokenize "((0,1)*, (2, 2)/) +;")))]

cbrActualA = TestList [testCbrActualAstA, testCbrActualA]
cbrActualB = TestList [testCbrActualAstB, testCbrActualB]
cbrActualC = TestList [testCbrActualAstC, testCbrActualC]
cbrActualD = TestList [testCbrActualAstD, testCbrActualD]
cbrActualE = TestList [testCbrActualAstE, testCbrActualE]
konto = TestList [testKontoAst, testKonto]
testAll = TestList [cbrActualA,cbrActualB, cbrActualC,cbrActualD, cbrActualE, konto, mineEgeneTester]
