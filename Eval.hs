module Eval where

import Expr

eval :: Expr -> Expr
eval (Lam b) = Lam (eval b)
eval (App f a) =
  case eval f of
    Lam b -> eval $ subst a b
    f -> App f (eval a)
eval x = x

subst :: Expr -> Expr -> Expr
subst x = go 0
  where
  go i (App f a) = App (go i f) (go i a)
  go i (Bound i') =
    case compare i i' of
      LT -> Bound (i' - 1)
      EQ -> shift i x
      GT -> Bound i'
  go i (Lam b) = Lam (go (i + 1) b)
  go _ x = x

shift :: Int -> Expr -> Expr
shift n = go 0
  where
  go i (App f a) = App (go i f) (go i a)
  go i (Bound i') | i <= i' = Bound (i' + n)
  go i (Lam b) = Lam (go (i + 1) b)
  go _ x = x
