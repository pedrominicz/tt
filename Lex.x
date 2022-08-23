{
module Lex (Token(..), pretty, tokenize) where

import Monad
import qualified Expr as E
}

%wrapper "basic"

$nl = [\n\r\f]
$alpha = [A-Za-z]
$digit = [0-9]

@skip = $nl* [\ \t\v]+
@name = [$alpha] [$alpha $digit _]*

tokens :-
  @skip         ;
  @name         { Name }
  "\" | "λ"     { const Lam }
  ","           { const Comma }
  "("           { const LParen }
  ")"           { const RParen }
  "="           { const Equal }
  $nl+          { const Newline }
  "--" [^$nl]*  ;

{
data Token
  = Name E.Name
  | Lam
  | Comma
  | LParen
  | RParen
  | Equal
  | Newline

pretty :: Token -> String
pretty (Name x) = x
pretty Lam = "λ"
pretty Comma = ","
pretty LParen = "("
pretty RParen = ")"
pretty Equal = "="
pretty Newline = "end of line"

tokenizeError :: AlexInput -> M [Token]
tokenizeError (_, _, str) = failure $ "unexpected '" ++ [head str] ++ "'"

tokenize :: String -> M [Token]
tokenize str = go ('\n', [], str)
  where
  go :: AlexInput -> M [Token]
  go input@(_, _, str) =
    case alexScan input 0 of
      AlexEOF -> return []
      AlexError input -> tokenizeError input
      AlexSkip input _ -> go input
      AlexToken input len act -> (act (take len str) :) <$> go input
}
