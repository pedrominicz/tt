module Syntax where

type Name = String

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  deriving (Eq)

instance Show Expr where
  show = go
    where
    go (Var v) = v
    go (Lam v b) = "\\" ++ v ++ "." ++ go b
    go (App f a) = go f ++ parens a

    parens (Var v) = v
    parens x = "(" ++ go x ++ ")"
