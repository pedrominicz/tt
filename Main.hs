module Main (main) where

import Expr
import Eval
import Parse

import System.Exit
import System.IO

prompt :: IO String
prompt = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof
    then exitSuccess
    else getLine

main :: IO ()
main = do
  e <- parse <$> prompt
  case e of
    Just e -> putStrLn $ pretty (eval e)
    Nothing -> putStrLn "?"
  main
