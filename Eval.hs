module Eval (eval) where

import Expr
import Monad

shift :: Int -> Expr -> Expr
shift s = go 0
  where
  go :: Int -> Expr -> Expr
  go k (Var x) =
    if x >= k
      then Var (x + s)
      else Var x
  go _ (Const x) = Const x
  go k (Lam b) = Lam (go (k + 1) b)
  go k (App f a) = App (go k f) (go k a)

subst :: Expr -> Expr -> Expr
subst s = go 0
  where
  go :: Int -> Expr -> Expr
  go k (Var x) =
    case compare x k of
      LT -> Var x
      EQ -> shift k s
      GT -> Var (x - 1)
  go _ (Const x) = Const x
  go k (Lam b) = Lam (go (k + 1) b)
  go k (App f a) = App (go k f) (go k a)

eval :: Expr -> M Expr
eval (Var x) = return (Var x)
eval (Const x) = do
  e <- valueOf x
  case e of
    Just e -> eval e
    Nothing -> return (Const x)
eval (Lam b) = Lam <$> eval b
eval (App f a) = do
  f <- eval f
  case f of
    Lam b -> eval (subst a b)
    f -> App f <$> eval a
