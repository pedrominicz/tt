module Syntax where

type Name = String

data Expr
    = Var Name
    | Lam Name Expr
    | App Expr Expr

instance Show Expr where
    show = go
        where
        go (Var x) = x
        go (Lam x body) = "\\" ++ x ++ "." ++ go body
        go (App x y) = parens x ++ go y

        parens (Var x) = x
        parens x = "(" ++ go x ++ ")"
