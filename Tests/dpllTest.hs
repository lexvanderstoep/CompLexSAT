-- Created by Lex van der Stoep on 15/03/2018.
-- Contains tests for the DPLL implementation.
-- The tests where taken from the lecture Logic & Proof (Cambridge, Part IB) by Lawrence C Paulson, and
-- the note "Seventy-Five Problems for Testing Automatic Theorem Provers" by F.J. Pelletier.
module Main where
import DPLL
import Test.HUnit

runGoal s = (validFormulaS s) == Valid
failGoal s = (validFormulaS s) /= Valid

satTests = TestList 
    [
     TestCase (assertBool "Absorptive &" (runGoal "P & P <-> P")),
     TestCase (assertBool "Absorptive |" (runGoal "P | P <-> P")),
     TestCase (assertBool "Commutative &" (runGoal "P & Q <-> Q & P")),
     TestCase (assertBool "Commutative |" (runGoal "P | Q <-> Q | P")),
     TestCase (assertBool "Associative &" (runGoal "(P & Q) & R <-> P & (Q & R)")),
     TestCase (assertBool "Associative |" (runGoal "(P | Q) | R  <->  P | (Q | R)")),
     TestCase (assertBool "Distributive | &" (runGoal "(P & Q) | R  <-> (P | R) & (Q | R)")),
     TestCase (assertBool "Distributive & |" (runGoal "(P | Q) & R  <-> (P & R) | (Q & R)")),
     TestCase (assertBool "Implication law I" (runGoal "(P|Q-->R) <-> (P-->R) & (Q-->R)")),
     TestCase (assertBool "Implication law II" (runGoal "(P&Q-->R)<->(P-->(Q-->R))")),
     TestCase (assertBool "Implication law III" (runGoal "(P-->Q&R) <-> (P-->Q)&(P-->R)")),
     TestCase (assertBool "Classic I" (runGoal "P|Q --> P | -P&Q")),
     TestCase (assertBool "Classic II" (runGoal "((P-->Q)-->Q) <-> P|Q")),
     TestCase (assertBool "Classic III" (runGoal "(P-->Q)&(-P-->R)-->(P&Q | R)")),
     TestCase (assertBool "Classic IV" (runGoal "P&Q | -P&R <-> (P-->Q)&(-P-->R)")),
     TestCase (assertBool "Classic V" (runGoal "(P-->Q) | (P-->R) <-> (P --> Q | R)")),
     TestCase (assertBool "Classic VI" (runGoal "(P<->Q) <-> (Q<->P)")),
     TestCase (assertBool "Pelletier I" (runGoal "(P-->Q) <-> (-Q --> -P)")),
     TestCase (assertBool "Pelletier II" (runGoal "--P <-> P")),
     TestCase (assertBool "Pelletier III" (runGoal "-(P-->Q) --> (Q-->P)")),
     TestCase (assertBool "Pelletier IV" (runGoal "(-P-->Q) <-> (-Q --> P)")),
     TestCase (assertBool "Pelletier V" (runGoal "((P|Q) --> (P|R)) --> (P|(Q-->R))")),
     TestCase (assertBool "Pelletier VI" (runGoal "P | -P")),
     TestCase (assertBool "Pelletier VII" (runGoal "P | ---P")),
     TestCase (assertBool "Pelletier VIII.1" (runGoal "((P-->Q) --> P) --> P")),
     TestCase (assertBool "Pelletier VIII.2" (runGoal "(-P-->P) --> P")),
     TestCase (assertBool "Pelletier IX" (runGoal "((P|Q) & (-P|Q) & (P|-Q)) --> - (-P | -Q)")),
     TestCase (assertBool "Pelletier X" (runGoal "((Q-->R) & (R --> P&Q) & (P --> Q|R)) --> (P<->Q)")),
     TestCase (assertBool "Pelletier XI" (runGoal "P<->P")),
     TestCase (assertBool "Pelletier XII" (runGoal "((P<->Q)<->R) <-> (P<->(Q<->R))")),
     TestCase (assertBool "Pelletier XIII" (runGoal "P | (Q & R) <-> (P|Q) & (P|R)")),
     TestCase (assertBool "Pelletier XIV" (runGoal "(P<->Q) <-> ((Q|-P) & (-Q|P))")),
     TestCase (assertBool "Pelletier XV" (runGoal "(P-->Q) <-> (-P|Q)")),
     TestCase (assertBool "Pelletier XVI" (runGoal "(P-->Q) | (Q-->P)")),
     TestCase (assertBool "Pelletier XVII" (runGoal "(P&(Q-->R)-->S) <-> ((-P | Q | S) & (-P | -R | S))")),
     TestCase (assertBool "Fail I" (failGoal "(P | Q --> R) <-> (P --> (Q --> R))")),
     TestCase (assertBool "Fail II" (failGoal "(P-->Q) <-> (Q-->-P)")),
     TestCase (assertBool "Fail III" (failGoal "-(P-->Q) --> (Q<->P)")),
     TestCase (assertBool "Fail IV" (failGoal "((P-->Q)-->Q)-->P")),
     TestCase (assertBool "Fail V" (failGoal "((P|Q) & (-P|Q) & (P|-Q)) --> -(-P|Q)")),
     TestCase (assertBool "Fail VI" (failGoal "((P & (Q<->R))<->S)  <->  ((-P | Q | S) & (-P | -R | S))"))]
main = do runTestTT satTests
