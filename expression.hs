-- Created by Lex van der Stoep on 01/02/2018.
-- Contains the datatypes representing Formulas.
-- Also can evaluate formulas and convert them to Conjunctive Normal Form.

module Expression where

data Formula = Var Char
              |And Formula Formula
              |Or Formula Formula
              |Not Formula
              |Impl Formula Formula
              |Const Bool
              
printF :: Formula -> String
printF (Var c) = [c]
printF (And a b) = "(" ++ (printF a) ++ " & " ++ (printF b) ++ ")"
printF (Or a b) = "(" ++ (printF a) ++ " | " ++ (printF b) ++ ")"
printF (Not a) = "-" ++ (printF a)
printF (Impl a b) = (printF a) ++ " => " ++ (printF b)
printF (Const True) = "T"
printF (Const False) = "F"


-- eval evaluates a formula.
eval :: Formula -> Bool
eval (Var c) = error ("Unbound variable " ++ [c] ++ " found")
eval (And e1 e2) = (eval e1) && (eval e2)
eval (Or e1 e2) = (eval e1) || (eval e2)
eval (Not e) = not(eval e)
eval (Impl e1 e2) = not(eval e1) || (eval e2)
eval (Const b) = b


-- toCNF converts a formula to conjunctive normal form (CNF)
toCNF :: Formula -> Formula
toCNF formula = step3 (step2 (step1 formula))
    where
        step1 (Impl a b) = Or (Not (step1 a)) (step1 b)
        step1 (Var a) = Var a
        step1 (Or a b) = Or (step1 a) (step1 b)
        step1 (And a b) = And (step1 a) (step1 b)
        step1 (Not a) = Not (step1 a)
        step1 (Const b) = Const b
        
        step2 (Not (Not a)) = step2 a
        step2 (Not (And a b)) = Or (step2 (Not a)) (step2 (Not b))
        step2 (Not (Or a b)) = And (step2 (Not a)) (step2 (Not b))
        step2 (Not a) = Not (step2 a)
        step2 (Var a) = Var a
        step2 (Or a b) = Or (step2 a) (step2 b)
        step2 (And a b) = And (step2 a) (step2 b)
        step2 (Const b) = Const b
        
        step2 :: Formula -> Formula
        step3 (Or a (And b c)) = And (step3 (Or a b)) (step3 (Or a c))
        step3 (Or (And b c) a) = step3 (Or a (And b c))
        step3 (Or a b) = Or (step3 a) (step3 b)
        step3 (Var a) = Var a
        step3 (And a b) = And (step3 a) (step3 b)
        step3 (Not a) = Not (step3 a)
        step3 (Const b) = Const b
