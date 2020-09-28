{
module Parse (parse) where

import Lex
import Syntax
}

%tokentype { Token }

%monad { Maybe }
%error { const Nothing }

%name parseExpr

%token
  '\\'  { Lambda }
  '.'   { Dot }
  '('   { OpenParen }
  ')'   { CloseParen }
  var   { Variable $$ }
%%

expr
  : '\\' var lambda   { Lam $2 $3 }
  | application       { $1 }

lambda
  : var lambda        { Lam $1 $2 }
  | '.' expr          { $2 }

application
  : application atom  { App $1 $2 }
  | atom              { $1 }

atom
  : '(' expr ')'      { $2 }
  | var               { Var $1 }

{
parse :: String -> Maybe Expr
parse str = do
  tokens <- tokenize str
  parseExpr tokens
}
