module Test where

import Eval
import Parse
import Syntax

import Data.Functor
import Data.List
import System.Exit

s :: Expr
s = Lam "x" (Lam "y" (Lam "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

k :: Expr
k = Lam "x" (Lam "y" (Var "x"))

test0 :: Bool
test0 = parse "\\x.\\y.x" == Just k

test1 :: Bool
test1 = parse "\\xyz.xz(yz)" == Just s

test2 :: Bool
test2 = parse "(\\x.x)(\\y.y)" == Just (App (Lam "x" (Var "x")) (Lam "y" (Var "y")))

test3 :: Bool
test3 = eval (App (App s k) k) == Lam "z" (Var "z")

tests :: [Bool]
tests =
  [ test0
  , test1
  , test2
  , test3
  ]

errors :: [String]
errors = elemIndices False tests <&> \i ->
  unwords ["Test", show i, "failed."]

main :: IO ()
main = do
  sequence_ $ putStrLn <$> errors
  if null errors
    then putStrLn "All tests were successful."
    else exitFailure
