{
module Lex (Token(..), pretty, tokenize) where

import qualified Expr as E

import Control.Monad.Except
}

%wrapper "basic"

@name = [a-z]

tokens :-
  $white+       ;
  @name         { Name . head }
  "\" | "λ"     { const Lam }
  "."           { const Dot }
  "("           { const LParen }
  ")"           { const RParen }
  "--" [^\n\r]* ;

{
data Token
  = Name E.Name
  | Lam
  | Dot
  | LParen
  | RParen

pretty :: Token -> String
pretty (Name x) = [x]
pretty Lam = "λ"
pretty Dot = "."
pretty LParen = "("
pretty RParen = ")"

tokenizeError :: AlexInput -> Except String [Token]
tokenizeError (_, _, str) = throwError ("unexpected '" ++ [head str] ++ "'")

tokenize :: String -> Except String [Token]
tokenize str = go ('\n', [], str)
  where
  go :: AlexInput -> Except String [Token]
  go input@(_, _, str) =
    case alexScan input 0 of
      AlexEOF -> return []
      AlexError input -> tokenizeError input
      AlexSkip input _ -> go input
      AlexToken input len act -> (act (take len str) :) <$> go input
}
