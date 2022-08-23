module Main (main) where

import Expr
import Monad
import Eval
import Parse

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import System.Environment
import System.Exit
import System.IO

check :: Expr -> M ()
check (Var _) = return ()
check (Const x) = do
  defined <- defined x
  when (not defined) . failure $ "name '" ++ x ++ "' is not defined"
check (Lam b) = check b
check (App f a) = do
  check f
  check a

command :: Command -> M ()
command (Eval e) = do
  e <- eval e
  liftIO . putStrLn $ pretty e
command (Def x e) = do
  defined <- defined x
  when defined . failure $ "multiple definitions of '" ++ x ++ "'"
  check e
  define x e

prompt :: M String
prompt = liftIO $ do
  putStr "> "
  hFlush stdout
  eof <- liftIO isEOF
  if eof
    then exitSuccess
    else getLine

repl :: M ()
repl = do
  recover $ do
    e <- prompt >>= parse
    case e of
      Just cmd -> command cmd
      Nothing -> return ()
  repl

file :: String -> M ()
file f = do
  f <- liftIO $ readFile f
  cmds <- parseFile f
  for_ cmds command

main :: IO ()
main = runM (liftIO getArgs >>= traverse file >> repl)
