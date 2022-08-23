module Expr where

import Data.List

type Name = Char

data Expr
  = App Expr Expr
  | Bound Int
  | Free Name
  | Lam Expr

free :: Expr -> [Name]
free (App f a) = free f ++ free a
free (Bound _) = []
free (Free x) = [x]
free (Lam b) = free b

instance Show Expr where
  show expr = go 0 expr
    where
    go n (App f a) = parens n f ++ parens' n a
    go n (Bound i) = [names !! (n - i - 1)]
    go _ (Free x) = [x]
    go n (Lam b) = "\\" ++ [names !! n] ++ "." ++ go (n + 1) b

    parens n (Lam b) = "(" ++ go n (Lam b) ++ ")"
    parens n x = go n x

    parens' n (Bound i) = go n (Bound i)
    parens' n (Free x) = go n (Free x)
    parens' n x = "(" ++ go n x ++ ")"

    -- This explodes if indexed beyond 25 or less.
    names = ['a'..'z'] \\ free expr
