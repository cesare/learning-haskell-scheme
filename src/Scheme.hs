module Scheme where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
  | List[LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool


spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"
