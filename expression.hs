-- Created by Lex van der Stoep on 01/02/2018.
-- Contains the datatypes representing Formulas.
-- Also can evaluate formulas, convert them to Conjunctive Normal Form and
-- parse formulas.

module Expression where

data Formula = Var String
              |And Formula Formula
              |Or Formula Formula
              |Not Formula
              |Impl Formula Formula
              |BiImpl Formula Formula
              |Const Bool deriving Eq
              
instance Show Formula where
  show (Var c) = c
  show (And a b) = "(" ++ (show a) ++ " ∧ " ++ (show b) ++ ")"
  show (Or a b) = "(" ++ (show a) ++ " ∨ " ++ (show b) ++ ")"
  show (Not a) = "-" ++ (show a)
  show (Impl a b) = "(" ++ (show a) ++ " --> " ++ (show b) ++ ")"
  show (BiImpl a b) = "(" ++ (show a) ++ "<->" ++ (show b) ++ ")"
  show (Const True) = "T"
  show (Const False) = "F"




-- EVALUATION
-- eval evaluates a formula.
eval :: Formula -> Bool
eval (Var c) = error ("Unbound variable " ++ c ++ " found")
eval (And e1 e2) = (eval e1) && (eval e2)
eval (Or e1 e2) = (eval e1) || (eval e2)
eval (Not e) = not(eval e)
eval (Impl e1 e2) = not(eval e1) || (eval e2)
eval (BiImpl e1 e2) = (eval e1)==(eval e2)
eval (Const b) = b




-- CNF CONVERSION
-- apply a function f to x until (f x) is equal to x, at that point return x
applyIterate f x = if ((f x) == x) then x else applyIterate f (f x)

-- toCNF converts a formula to conjunctive normal form (CNF)
toCNF :: Formula -> Formula
toCNF formula = applyIterate step3 (step2 $ step1 formula)
    where
        -- convert Impl and BiImpl connectives
        step1 (Impl a b) = Or (Not (step1 a)) (step1 b)
        step1 (BiImpl a b) = Or (And (step1 a) (step1 b)) (And (Not (step1 a)) (Not (step1 b)))
        step1 (Var a) = Var a
        step1 (Or a b) = Or (step1 a) (step1 b)
        step1 (And a b) = And (step1 a) (step1 b)
        step1 (Not a) = Not (step1 a)
        step1 (Const b) = Const b
        -- push in the negations
        step2 (Not (Not a)) = step2 a
        step2 (Not (And a b)) = Or (step2 (Not a)) (step2 (Not b))
        step2 (Not (Or a b)) = And (step2 (Not a)) (step2 (Not b))
        step2 (Not a) = Not (step2 a)
        step2 (Var a) = Var a
        step2 (Or a b) = Or (step2 a) (step2 b)
        step2 (And a b) = And (step2 a) (step2 b)
        step2 (Const b) = Const b
        -- push in disjunctions
        step3 (Or a (And b c)) = And (step3 (Or a b)) (step3 (Or a c))
        step3 (Or (And b c) a) = And (step3 (Or b a)) (step3 (Or c a))
        step3 (Or a b) = Or (step3 a) (step3 b)
        step3 (Var a) = Var a
        step3 (And a b) = And (step3 a) (step3 b)
        step3 (Not a) = Not (step3 a)
        step3 (Const b) = Const b




-- PARSING
data Token = SYMB String | AND | OR | NOT | IMPL | BIIMPL | TRUE | FALSE | LPAR | RPAR deriving (Eq, Show)

-- checks if the first string starts with the second string
startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith (c:cs) (s:ss) = (c==s) && (startsWith cs ss)

-- equal to (takeWhile, dropWhile)
splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile _ [] = ([], [])
splitWhile f (x:xs) =
    if (f x) then
        let (l, r) = splitWhile f xs
        in (x:l, r)
    else
        ([], x:xs)

isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

-- turn the string into a list of tokens
tokenise :: String -> [Token]
tokenise "" = []
tokenise (' ':cs) = tokenise cs
tokenise ('&':cs) = AND:(tokenise cs)
tokenise ('|':cs) = OR:(tokenise cs)
tokenise ('t':cs) = TRUE:(tokenise cs)
tokenise ('f':cs) = FALSE:(tokenise cs)
tokenise ('(':cs) = LPAR:(tokenise cs)
tokenise (')':cs) = RPAR:(tokenise cs)
tokenise s =
    if startsWith s "-->" then
        IMPL:(tokenise (drop 3 s))
    else if startsWith s "<->" then
        BIIMPL:(tokenise (drop 3 s))
    else if startsWith s "-" then
        NOT:(tokenise (drop 1 s))
    else let (l, r) = splitWhile isUpper s
         in if (l == "") then
                error ("Unknown symbol " ++ [(head r)] ++ " recognised.")
            else
                (SYMB l):(tokenise r)

-- The parsing iteratively converts substrings to formulae.
-- The left represents underparsed tokens, and right represents parsed tokens.
type IterParse = Either Token Formula

-- Takes the tokens and iteratively parses them into a formula
iterParse :: [IterParse] -> [IterParse]
iterParse tks = step5 $ step4 $ step3 $ step2 $ step1 $ step0 tks
          where
            -- parse symbols and constants
            step0 [] = []
            step0 ((Left (SYMB s)):tks) = (Right (Var s)):(step0 tks)
            step0 ((Left TRUE):tks) = (Right (Const True)):(step0 tks)
            step0 ((Left FALSE):tks) = (Right (Const False)):(step0 tks)
            step0 (tk:tks) = tk:(step0 tks)
            -- parse parentheses by fully parsing the tokens inside the
            -- parentheses before parsing the rest of the tokens
            getPar ((Left LPAR):tks) n xs = getPar tks (n+1) ((Left LPAR):xs)
            getPar ((Left RPAR):tks) 0 xs = (reverse xs, tks)
            getPar ((Left RPAR):tks) n xs = getPar tks (n-1) ((Left RPAR):xs)
            getPar (tk:tks) n xs = getPar tks n (tk:xs)
            step1 [] = []
            step1 ((Left LPAR):tks) =
                let split = getPar tks 0 []
                in (iterParse $ fst split)++(step1 $ snd split)
            step1 (tk:tks) = tk:(step1 tks)
            -- parse negation
            appToHead f ((Right r):tl) = (Right (f r)):tl
            step2 [] = []
            step2 ((Left NOT):tks) = 
                let ((Right e):ps) = step2 tks
                in (Right (Not e)):ps
            step2 (tk:tks) = tk:(step2 tks)
            -- parse conjunction
            step3 [] = []
            step3 ((Right e1):(Left AND):(Right e2):tks) =
                step3 ((Right (And e1 e2)):tks)
            step3 (tk:tks) = tk:(step3 tks)
            -- parse disjunction
            step4 [] = []
            step4 ((Right e1):(Left OR):(Right e2):tks) =
                step4 ((Right (Or e1 e2)):tks)
            step4 (tk:tks) = tk:(step4 tks)
            -- parse (bi-)implication
            step5 [] = []
            step5 ((Right e1):(Left IMPL):(Right e2):tks) =
                step5 ((Right (Impl e1 e2)):tks)
            step5 ((Right e1):(Left BIIMPL):(Right e2):tks) =
                step5 ((Right (BiImpl e1 e2)):tks)
            step5 [(Right e)] = [(Right e)]

parseTokens :: [Token] -> Formula
parseTokens tks = 
    let [(Right f)] = iterParse (map Left tks)
    in f

parse :: String -> Formula
parse s = parseTokens $ tokenise s