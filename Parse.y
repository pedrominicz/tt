{
module Parse (parse, parseFile) where

import Monad
import qualified Expr as E
import qualified Lex as L

import Data.List
}

%expect 0

%tokentype { L.Token }

%monad { M }
%error { parseError }

%name line line
%name file file

%token
  name          { L.Name $$ }
  num           { L.Num $$ }
  'λ'           { L.Lam }
  ','           { L.Comma }
  '('           { L.LParen }
  ')'           { L.RParen }
  '='           { L.Equal }
  '\n'          { L.Newline }
%%

line :: { Maybe E.Command }
  : name '=' expr               { Just (E.Def $1 (nameless $3)) }
  | expr                        { Just (E.Eval (nameless $1)) }
  | {- empty -}                 { Nothing }

file :: { [E.Command] }
  : name '=' expr '\n' file     { E.Def $1 (nameless $3) : $5 }
  | {- empty -}                 { [] }

-- Expressions

expr :: { Expr }
  : 'λ' names1 ',' expr         { foldr Lam $4 $2 }
  | application                 { $1 }

application :: { Expr }
  : application simple          { App $1 $2 }
  | simple                      { $1 }

simple :: { Expr }
  : name                        { Name $1 }
  | num                         { Num $1 }
  | '(' expr ')'                { $2 }

names1 :: { [E.Name] }
  : name                        { [$1] }
  | name names1                 { $1 : $2 }

{
data Expr
  = Name E.Name
  | Num Int
  | Lam E.Name Expr
  | App Expr Expr

nameless :: Expr -> E.Expr
nameless = go []
  where
  go :: [E.Name] -> Expr -> E.Expr
  go ctx (Name x) =
    case elemIndex x ctx of
      Just x -> E.Var x
      Nothing -> E.Const x
  go _ (Num x) = E.Lam (E.Lam (foldr E.App (E.Var 0) (replicate x (E.Var 1))))
  go ctx (Lam x b) = E.Lam (go (x : ctx) b)
  go ctx (App f a) = E.App (go ctx f) (go ctx a)

parseError :: [L.Token] -> M a
parseError [] = failure "unexpected end of file"
parseError (tk : _) = failure $ "unexpected '" ++ L.pretty tk ++ "'"

parse :: String -> M (Maybe E.Command)
parse str = L.tokenize str >>= line

parseFile :: String -> M [E.Command]
parseFile str = L.tokenize str >>= file
}
