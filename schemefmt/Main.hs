{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Monad.Except
import Data.Foldable
import System.Environment
import System.IO

import Language.Scheme.Pretty
import Language.Scheme.Reader

data FormatError = FormatError deriving (Eq, Show)
type FormatM = ExceptT FormatError IO

-- FIXME: Don't ignore blank lines
formatString :: String -> FormatM String
formatString s = do
  case readExprList s of
    Left _ -> throwError FormatError
    Right exprs -> pure $ prettyPrint exprs

-- FIXME: Add IO exception handling
formatFile :: FilePath -> IO ()
formatFile path = do
  contents <- readFile path
  res <- runExceptT (formatString contents)
  case res of
    Left e  -> print e
    Right newContents -> writeFile path newContents

usage :: String
usage = "Usage: schemefmt [path ...]"

main :: IO ()
main = do
  args <- getArgs
  if null args
     then putStrLn usage
     else traverse_ formatFile args

