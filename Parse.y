{
module Parse (parse) where

import Lex
import Syntax
}

%tokentype { Token }

%monad { Maybe } { (>>=) } { return }
%error { const Nothing }

%name parseExpr

%token
  '\\'  { Lambda }
  '.'   { Dot }
  '('   { OpenParen }
  ')'   { CloseParen }
  var   { Variable $$ }
%%

Expr
  : '\\' var Lambda   { Lam $2 $3 }
  | Application       { $1 }

Lambda
  : var Lambda        { Lam $1 $2 }
  | '.' Expr          { $2 }

Application
  : Application Atom  { App $1 $2 }
  | Atom              { $1 }

Atom
  : '(' Expr ')'      { $2 }
  | var               { Var $1 }

{
parse :: String -> Maybe Expr
parse str = do
  tokens <- tokenize str
  parseExpr tokens
}
