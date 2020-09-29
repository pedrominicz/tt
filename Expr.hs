module Expr where

import Data.List

type Name = Char

data Expr
  = App Expr Expr
  | Bound Int
  | Free Name
  | Lam Expr
  deriving (Eq)

free :: Expr -> [Name]
free (App f a) = free f ++ free a
free (Bound x) = []
free (Free x)  = [x]
free (Lam b)   = free b

instance Show Expr where
  show expr = go 0 expr
    where
    go n (App f a) = go n f ++ parens n a
    go n (Bound x) = [names !! (n - x - 1)]
    go n (Free x)  = [x]
    go n (Lam b)   = "\\" ++ [names !! n] ++ "." ++ go (n + 1) b

    parens n (Bound x) = go n (Bound x)
    parens n (Free x)  = go n (Free x)
    parens n x         = "(" ++ go n x ++ ")"

    -- This explodes if indexed beyond 25 or less.
    names = ['a'..'z'] \\ free expr
