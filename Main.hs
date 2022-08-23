module Main (main) where

import Expr
import Eval
import Parse

import Control.Monad.Except
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
  case runExcept e of
    Left e -> hPutStrLn stderr e
    Right (Just e) -> putStrLn $ pretty (eval e)
    Right Nothing -> return ()
  main
