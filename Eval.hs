module Eval where

import Syntax

eval :: Expr -> Expr
eval (Lam v b) = Lam v (eval b)
eval (App f a) =
  case eval f of
    Lam v b -> eval $ subst v a b
    f -> App f (eval a)
eval x = x

subst :: Name -> Expr -> Expr -> Expr
subst v x (Var v')   | v == v' = x
subst v x (Lam v' b) | v /= v' = Lam v' (subst v x b)
subst v x (App f a)            = App (subst v x f) (subst v x a)
subst _ _ x                    = x
