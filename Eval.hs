module Eval where

import Expr

eval :: Expr -> Expr
eval (Lam b) = Lam (eval b)
eval (App f a) =
  case eval f of
    Lam b -> eval $ subst 0 a b
    f -> App f (eval a)
eval x = x

subst :: Int -> Expr -> Expr -> Expr
subst i x (App f a) = App (subst i x f) (subst i x a)
subst i x (Bound i') | i == i' = x
subst i x (Lam b) = Lam (subst (i + 1) x b)
subst _ _ x = x
