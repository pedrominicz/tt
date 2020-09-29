{
module Parse (parse) where

import Lex
import Syntax

import Control.Applicative
import Control.Monad.Reader
}

%tokentype { Token }

%monad { ReaderT [Name] Maybe }
%error { const empty }

%name parseExpr

%token
  '\\'  { Lambda }
  '.'   { Dot }
  '('   { OpenParen }
  ')'   { CloseParen }
  var   { Variable $$ }
%%

expr
  : '\\' var lambda   { Lam $3 }
  | application       { $1 }

lambda
  : var lambda        { Lam $2 }
  | '.' expr          { $2 }

application
  : application atom  { App $1 $2 }
  | atom              { $1 }

atom
  : '(' expr ')'      { $2 }
  | var               { Free $1 }

{
parse :: String -> Maybe Expr
parse str = flip runReaderT [] $ do
  tokens <- lift $ tokenize str
  parseExpr tokens
}
