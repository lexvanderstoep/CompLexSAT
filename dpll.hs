-- Created by Lex van der Stoep on 12/02/2018.
-- Contains functions to run the DPLL method.
-- The function satisfiable runs the entire DPLL method on a set of clauses.
module DPLL where
import Expression

data Literal = VAR Char
             | NVAR Char deriving (Show, Eq)

type Clause = [Literal]

-- BASIC FUNCTIONS
contains [] _ = False
contains (x:xs) y = if x==y then True
                    else contains xs y

remove [] _ = []
remove (x:xs) y = if x==y then remove xs y
                  else x:(remove xs y)
                  
containsAny :: Eq a => [a] -> [a] -> Bool
containsAny [] _ = False
containsAny _ [] = False
containsAny (l:ls) (c:cs) = if (l == c) then True else (containsAny (l:ls) cs) || (containsAny ls (c:cs))

-- Convert a CNF formula to a list of clauses 
toList :: Formula -> [Clause]
toList (Var c) = [[VAR c]]
toList (Not (Var c)) = [[NVAR c]]
toList (And a b) = (toList a)++(toList b)
toList (Or a b) = [toClause (Or a b)]
    where toClause (Or a b) = (toClause a)++(toClause b)
          toClause (Var c) = [VAR c]
          toClause (Not (Var c)) = [NVAR c]
          toClause _ = error "The formula is not in CNF"
toList _ = error "The formula is not in CNF"
--

                
-- Remove all tautological clauses from a list of clauses
removeTautologies :: [Clause] -> [Clause]
removeTautologies cs = filter (not.tautology) cs
    where
        tautology [] = False
        tautology ((VAR c):cs) = (contains cs (NVAR c)) || (tautology cs)
        tautology ((NVAR c):cs) = (contains cs (VAR c)) || (tautology cs)
--

-- Unit propagation
unitPropagation :: [Clause] -> [Clause]
unitPropagation cs = removeUnits cs cs
    where removeUnit _ [] = []
          removeUnit (VAR x) (c:cs)
            | contains c (VAR x) = removeUnit (VAR x) cs
            | contains c (NVAR x) = (remove c (NVAR x)):(removeUnit (VAR x) cs)
            | otherwise = c:(removeUnit (VAR x) cs)
          removeUnit (NVAR x) (c:cs)
            | contains c (NVAR x) = removeUnit (NVAR x) cs
            | contains c (VAR x) = (remove c (VAR x)):(removeUnit (NVAR x) cs)
            | otherwise = c:(removeUnit (NVAR x) cs)
    
          removeUnits [] cs' = cs'
          removeUnits ([l]:cs) cs'  = removeUnits cs (removeUnit l cs')
          removeUnits (_:cs) cs' = removeUnits cs cs'
--

-- Remove all clauses with pure literals
findAllOccurences :: Eq a => [[a]] -> [a] ->[a]
findAllOccurences [] ys = ys
findAllOccurences ([]:xss) ys = findAllOccurences xss ys
findAllOccurences ((x:xs):xss) ys = if not (contains ys x) then findAllOccurences (xs:xss) (x:ys) else findAllOccurences (xs:xss) ys

findPures :: [Literal] -> [Literal]
findPures [] = []
findPures ((VAR c):vs) = if (not (contains vs (NVAR c))) then (VAR c):(findPures vs)
                            else findPures (remove vs (NVAR c))
findPures ((NVAR c):vs) = if (not (contains vs (VAR c))) then (NVAR c):(findPures vs)
                             else findPures (remove vs (VAR c))

findPureLiterals :: [Clause] -> [Literal]
findPureLiterals cs = findPures allVariables
    where allVariables = findAllOccurences cs []

removePureLiterals :: [Clause] -> [Clause]
removePureLiterals cs = filter (\ls -> not (containsAny (findPureLiterals cs) ls)) cs
--

-- Case split a variable found in the clauses, setting it to True and False and checking for satisfiability
findVariable :: [Clause] -> Char
findVariable ([]:cs) = findVariable cs
findVariable (((VAR c):_):_) = c
findVariable (((NVAR c):_):_) = c

setValue :: [Clause] -> Char -> Bool -> [Clause]
setValue [] _ _ = []
setValue (c:cs) var True = if (contains c (VAR var)) then setValue cs var True
                           else if (contains c (NVAR var)) then (remove c (NVAR var)):(setValue cs var True)
                           else c:(setValue cs var True)
setValue (c:cs) var False = if (contains c (NVAR var)) then setValue cs var False
                            else if (contains c (VAR var)) then (remove c (VAR var)):(setValue cs var False)
                            else c:(setValue cs var False)

caseSplit :: [Clause] -> Bool -> [Clause]
caseSplit [] _ = []
caseSplit cs val = setValue cs var val
                   where var = findVariable cs

-- Check for solution
satisfiable :: [Clause] -> Bool
satisfiable [] = True
satisfiable cs = if (contains cs' []) then False
                 else (satisfiable (caseSplit cs' True)) || (satisfiable (caseSplit cs' False))
                 where cs' = removePureLiterals (unitPropagation (removeTautologies cs))