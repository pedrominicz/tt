module Test (main) where

import Expr

import Data.Foldable
import Data.List
import System.Exit

tests :: [Bool]
tests =
  [ "λ a, a" == pretty (Lam (Var 0))
  , "λ a, λ b, a" == pretty (Lam (Lam (Var 1)))
  , "λ b, λ c, a b" == pretty (Lam (Lam (App (Const "a") (Var 1))))
  , "λ a, λ b, λ c, a c (b c)" == pretty (Lam (Lam (Lam (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0))))))
  , "λ b, λ d, λ e, c b e (d (a e))" == pretty (Lam (Lam (Lam (App (App (App (Const "c") (Var 2)) (Var 0)) (App (Var 1) (App (Const "a") (Var 0)))))))
  , "(λ a, a) (λ a, a)" == pretty (App (Lam (Var 0)) (Lam (Var 0)))
  ]

errors :: [String]
errors = report <$> elemIndices False tests
  where
  report :: Int -> String
  report i = unwords ["Test", show (i + 1), "failed."]

main :: IO ()
main = do
  for_ errors putStrLn
  if null errors
    then putStrLn "All tests were successful."
    else exitFailure
