-- Created by Lex van der Stoep on 01/02/2018.
-- Contains the datatypes representing Expressions.

data Expr = Var Char
           |And Expr Expr
           |Or Expr Expr
           |Not Expr
           |Impl Expr Expr
           |T
           |F;

eval T = True;
eval F = False;
