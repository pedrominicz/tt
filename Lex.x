{
module Lex (Token(..), tokenize) where

import Syntax
}

%wrapper "basic"

$var = [a-z]
$eol = [\n]

tokens :-
  $eol    ;
  $white+ ;
  "--".*  ;
  \\      { \s -> Lambda }
  \.      { \s -> Dot }
  \(      { \s -> OpenParen }
  \)      { \s -> CloseParen }
  $var    { \s -> Variable s }

{
data Token
  = Lambda
  | Dot
  | OpenParen
  | CloseParen
  | Variable Name
  deriving (Show)

tokenize :: String -> Maybe [Token]
tokenize str = go ('\n', [], str)
  where
  go input@(_, _, str) =
    case alexScan input 0 of
      AlexEOF            -> Just []
      AlexError _        -> Nothing
      AlexSkip input len -> go input
      AlexToken input len action -> do
        rest <- go input
        return (action (take len str) : rest)
}
