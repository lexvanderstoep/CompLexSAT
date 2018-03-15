-- Created by Lex van der Stoep on 15/03/2018.
-- Contains tests for the DPLL implementation.
module Main where
import DPLL
import Test.HUnit

satTests = TestList 
    [-- Sat: {x}
     TestCase (assertEqual "SAT1" (satisfiable [[VAR 'x']]) True),
     -- Sat: {x} {y}
     TestCase (assertEqual "SAT2" (satisfiable [[VAR 'x'], [VAR 'y']]) True),
     -- Unsat: {-Q, R} {-R, P} {-R, Q} {-P, Q, R} {P, Q} {-P, -Q}
     TestCase (assertEqual "SAT3" (satisfiable [[NVAR 'q', VAR 'r'],[NVAR 'r', VAR 'p'], [NVAR 'r', VAR 'q'],[NVAR 'p', VAR 'q', VAR 'r'],[VAR 'p', VAR 'q'],[NVAR 'p', NVAR 'q']]) False),
     -- Sat: {P, Q} {-Q} {-R}
     TestCase (assertEqual "SAT4" (satisfiable [[VAR 'p', VAR 'q'], [NVAR 'q'], [NVAR 'r']]) True),
     -- Unsat: {P, Q} {-P} {-Q}
     TestCase (assertEqual "SAT5" (satisfiable [[VAR 'p', VAR 'q'], [NVAR 'p'], [NVAR 'q']]) False)]
main = do runTestTT satTests