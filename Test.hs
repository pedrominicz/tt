module Test where

import Expr

import Data.Functor
import Data.List
import System.Exit

test0, test1, test2, test3, test4 :: Bool
test0 = "\\a.a" == show (Lam (Bound 0))
test1 = "\\a.\\b.a" == show (Lam (Lam (Bound 1)))
test2 = "\\b.\\c.ab" == show (Lam (Lam (App (Free 'a') (Bound 1))))
test3 = "\\a.\\b.\\c.ac(bc)" == show (Lam (Lam (Lam (App (App (Bound 2) (Bound 0)) (App (Bound 1) (Bound 0))))))
test4 = "\\b.\\d.\\e.cbe(d(ae))" == show (Lam (Lam (Lam (App (App (App (Free 'c') (Bound 2)) (Bound 0)) (App (Bound 1) (App (Free 'a') (Bound 0)))))))
test5 = "(\\a.a)(\\a.a)" == show (App (Lam (Bound 0)) (Lam (Bound 0)))

tests :: [Bool]
tests = [test0, test1, test2, test3, test4]

errors :: [String]
errors = elemIndices False tests <&> \i ->
  unwords ["Test", show i, "failed."]

main :: IO ()
main = do
  sequence_ $ putStrLn <$> errors
  if null errors
    then putStrLn "All tests were successful."
    else exitFailure
