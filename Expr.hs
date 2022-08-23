module Expr (Name, Expr(..), Command(..), pretty) where

import Data.List

type Name = String

data Expr
  = Var Int
  | Const Name
  | Lam Expr
  | App Expr Expr

data Command = Def Name Expr | Eval Expr

free :: Expr -> [Name]
free (Var _) = []
free (Const x) = [x]
free (Lam b) = free b
free (App f a) = free f ++ free a

pretty :: Expr -> String
pretty e = expr 0 e
  where
  expr :: Int -> Expr -> String
  expr k (Lam b) = "λ " ++ names !! k ++ ", " ++ expr (k + 1) b
  expr k e = application k e

  application :: Int -> Expr -> String
  application k (App f a) = application k f ++ " " ++ simple k a
  application k e = simple k e

  simple :: Int -> Expr -> String
  simple k (Var x) = names !! (k - x - 1)
  simple _ (Const x) = x
  simple k e = "(" ++ expr k e ++ ")"

  -- This explodes if indexed beyond 25 (or less, depending on the number of
  -- free variables).
  names :: [Name]
  names = map return ['a'..'z'] \\ free e
