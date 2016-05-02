module Scheme.Parser
  ( readExpr
  , readExprList
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Scheme.Types

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment empty
  where lineComment  = L.skipLineComment ";"

symbol :: String -> Parser String
symbol = L.symbol sc

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow $ sc >> parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ sc >> endBy parseExpr sc

lispSymbolChar :: Parser Char
lispSymbolChar = oneOf "!$%&|*+-/:<=>?@^_~#"

parseString :: Parser LispVal
parseString = String <$> quotes (many (noneOf "\""))

parseAtom :: Parser LispVal
parseAtom = do first <- letterChar <|> lispSymbolChar
               rest <- many (letterChar <|> digitChar <|> lispSymbolChar)
               let atom = first : rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

-- FIXME: Recognize floating point numbers
parseNumber :: Parser LispVal
parseNumber = do sign <- optional $ oneOf "+-"
                 digits <- some digitChar
                 return $ case sign of
                            Just '-' -> Number . negate . read $ digits
                            otherwise -> Number . read $ digits

parseList :: Parser LispVal
parseList = List <$> endBy parseExpr sc

parseDottedList :: Parser LispVal
parseDottedList = DottedList <$> endBy parseExpr sc <* symbol "." <*> parseExpr

parseQuoted :: Parser LispVal
parseQuoted = do
    symbol "'"
    x <- parseExpr
    return $ List [Atom "quote", x]

parseDottedListOrList :: Parser LispVal
parseDottedListOrList = parens (try parseDottedList <|> parseList)

parseExpr :: Parser LispVal
parseExpr = try parseNumber
        <|> parseAtom
        <|> parseString
        <|> parseQuoted
        <|> parseDottedListOrList

