module Language.Scheme.Reader
  ( readExpr
  , readExprList
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import qualified Data.Array.IArray as IArray

import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Language.Scheme.Types

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment empty
  where lineComment  = L.skipLineComment ";"

symbol :: String -> Parser String
symbol = L.symbol sc

symbol' :: String -> Parser String
symbol' = L.symbol' sc

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> pure val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow $ sc >> schemeExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ sc >> endBy schemeExpr sc

schemeSymbolChar :: Parser Char
schemeSymbolChar = oneOf "!$%&|*+-/:<=>?@^_~#"

schemeChar :: Parser LispVal
schemeChar = do
  _  <- try (string "#\\")
  c  <- anyChar
  cs <- many (letterChar <|> digitChar)
  let chracterName = c:cs
  case chracterName of
    "space"   -> pure $ Char ' '
    "newline" -> pure $ Char '\n'
    [ch] -> pure $ Char ch
    _ -> fail $ "Unknown character name " ++ chracterName

schemeVector :: Parser LispVal
schemeVector = do
  _  <- char '#'
  exprs  <- parens $ endBy schemeExpr sc
  pure $ Vector $ IArray.listArray (0, length exprs - 1) exprs

schemeString :: Parser LispVal
schemeString = String <$> quotes (many (noneOf "\""))

schemeAtom :: Parser LispVal
schemeAtom = do first <- letterChar <|> schemeSymbolChar
                rest <- many (letterChar <|> digitChar <|> schemeSymbolChar)
                let atom = first : rest
                pure $ case atom of
                  "#t" -> Bool True
                  "#f" -> Bool False
                  otherwise -> Atom atom

-- FIXME: Recognize floating point numbers
schemeNumber :: Parser LispVal
schemeNumber = do sign <- optional $ oneOf "+-"
                  digits <- some digitChar
                  pure $ case sign of
                           Just '-' -> Number . negate . read $ digits
                           otherwise -> Number . read $ digits

schemeList :: Parser LispVal
schemeList = List <$> endBy schemeExpr sc

schemeDottedList :: Parser LispVal
schemeDottedList = DottedList <$> endBy schemeExpr sc <* symbol "." <*> schemeExpr

schemeQuoted :: Parser LispVal
schemeQuoted = do
    symbol "'"
    x <- schemeExpr
    pure $ List [Atom "quote", x]

schemeDottedListOrList :: Parser LispVal
schemeDottedListOrList = parens (try schemeDottedList <|> schemeList)

schemeExpr :: Parser LispVal
schemeExpr = try schemeNumber
           <|> schemeChar
           <|> try schemeVector
           <|> schemeAtom
           <|> schemeString
           <|> schemeQuoted
           <|> schemeDottedListOrList

