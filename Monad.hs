module Monad (M, runM, defined, define, failure, recover, valueOf) where

import Expr

import Control.Monad.Except
import Control.Monad.State
import System.Exit
import System.IO

import Data.Map (Map)
import qualified Data.Map as M

type M a = StateT (Map Name Expr) (ExceptT String IO) a

runM :: M a -> IO a
runM x = do
  x <- runExceptT (evalStateT x M.empty)
  case x of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right x -> return x

defined :: Name -> M Bool
defined x = do
  env <- get
  return $ M.member x env

define :: Name -> Expr -> M ()
define x e = modify $ M.insert x e

failure :: String -> M a
failure = throwError

recover :: M () -> M ()
recover x = x `catchError` \e -> liftIO $ hPutStrLn stderr e

valueOf :: Name -> M (Maybe Expr)
valueOf x = do
  env <- get
  return $ M.lookup x env
