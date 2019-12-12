module Eval where

import Syntax

eval :: Expr -> Expr
eval (Lam x body) = Lam x (eval body)
eval (App x y) =
    case eval x of
        Lam x body -> eval $ subst x y body
        x -> App x (eval y)
eval x = x

subst :: Name -> Expr -> Expr -> Expr
subst x y (Var x')
    | x == x' = y
subst x y (Lam x' body)
    | x /= x' = Lam x' (subst x y body)
subst x y (App x' y') = App (subst x y x') (subst x y y')
subst _ _ x = x
