{
module Parse (parse) where

import qualified Expr as E
import qualified Lex as L

import Data.List
}

%expect 0

%tokentype { L.Token }

%monad { Maybe }
%error { const Nothing }

%name parseExpr expr

%token
  name          { L.Name $$ }
  'λ'           { L.Lam }
  '.'           { L.Dot }
  '('           { L.LParen }
  ')'           { L.RParen }
%%

expr :: { Expr }
  : 'λ' names1 '.' expr         { foldr Lam $4 $2 }
  | application                 { $1 }

application :: { Expr }
  : application simple          { App $1 $2 }
  | simple                      { $1 }

simple :: { Expr }
  : name                        { Name $1 }
  | '(' expr ')'                { $2 }

names1 :: { [E.Name] }
  : name                        { [$1] }
  | name names1                 { $1 : $2 }

{
data Expr
  = Name E.Name
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
  go ctx (Lam x b) = E.Lam (go (x : ctx) b)
  go ctx (App f a) = E.App (go ctx f) (go ctx a)

parse :: String -> Maybe E.Expr
parse str = nameless <$> (L.tokenize str >>= parseExpr)
}
