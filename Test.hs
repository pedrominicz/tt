module Test (main) where

import Expr

import Data.Foldable
import Data.List
import System.Exit

tests :: [Bool]
tests =
  [ "λa.a" == pretty (Lam (Var 0))
  , "λa.λb.a" == pretty (Lam (Lam (Var 1)))
  , "λb.λc.ab" == pretty (Lam (Lam (App (Const 'a') (Var 1))))
  , "λa.λb.λc.ac(bc)" == pretty (Lam (Lam (Lam (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0))))))
  , "λb.λd.λe.cbe(d(ae))" == pretty (Lam (Lam (Lam (App (App (App (Const 'c') (Var 2)) (Var 0)) (App (Var 1) (App (Const 'a') (Var 0)))))))
  , "(λa.a)(λa.a)" == pretty (App (Lam (Var 0)) (Lam (Var 0)))
  ]

errors :: [String]
errors = report <$> elemIndices False tests
  where
  report :: Int -> String
  report i = unwords ["Test", show i, "failed."]

main :: IO ()
main = do
  for_ errors putStrLn
  if null errors
    then putStrLn "All tests were successful."
    else exitFailure
