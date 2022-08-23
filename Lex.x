{
module Lex (Token(..), tokenize) where

import qualified Expr as E
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

tokenize :: String -> Maybe [Token]
tokenize str = go ('\n', [], str)
  where
  go :: AlexInput -> Maybe [Token]
  go input@(_, _, str) =
    case alexScan input 0 of
      AlexEOF -> Just []
      AlexError _ -> Nothing
      AlexSkip input _ -> go input
      AlexToken input len act -> (act (take len str) :) <$> go input
}
