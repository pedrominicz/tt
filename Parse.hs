module Parse where

import Expr

import Control.Monad.Reader
import Data.List
import Text.Parsec hiding (parse)

type Parser = ParsecT String () (Reader [Name])

parse :: String -> Maybe Expr
parse s =
  case runReader (runParserT (whitespace *> expression <* eof) () "" s) [] of
    Left e -> Nothing
    Right e -> Just e

expression :: Parser Expr
expression = lambda <|> application

lambda :: Parser Expr
lambda = do
  try $ char '\\' *> whitespace
  xs <- many1 name
  char '.' *> whitespace
  b <- local (reverse xs ++) expression
  return $ foldr (const Lam) b xs

application :: Parser Expr
application = (parens expression <|> variable) `chainl1` return App

variable :: Parser Expr
variable = do
  env <- ask
  x <- name
  case elemIndex x env of
    Just i -> return $ Bound i
    Nothing -> return $ Free x

name :: Parser Name
name = satisfy (`elem` ['a'..'z']) <* whitespace

parens :: Parser a -> Parser a
parens p = between open close p
  where
  open = char '(' <* whitespace
  close = char ')' <* whitespace

whitespace :: Parser ()
whitespace = skipMany space
