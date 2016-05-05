{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Scheme.Types
  ( Env
  , liftThrows
  , LispError(..)
  , LispVal(..)
  , showVal
  , ThrowsError
  , EvalM(..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Array.IArray
import Data.IORef
import System.IO
import Text.Megaparsec

type ThrowsError = Either LispError

-- FIXME:: Use Data.StRef instead of Data.IORef.
type Env = IORef [(String, IORef LispVal)]
newtype EvalM a = EvalM {
        run :: (ReaderT Env (ExceptT LispError IO) a)
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError LispError)

liftThrows :: ThrowsError a -> EvalM a
liftThrows (Left err) = EvalM $ lift $ throwError err
liftThrows (Right val) = EvalM $ lift $ return val

data LispVal = Unspecified
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Char Char
             | String String
             | Vector (Array Int LispVal)
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> EvalM LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env}

instance Eq LispVal where
  (Bool arg1) == (Bool arg2) = arg1 == arg2
  (Number arg1) == (Number arg2) = arg1 == arg2
  (Char arg1) == (Char arg2) = arg1 == arg2
  (String arg1) == (String arg2) = arg1 == arg2
  (Vector arg1) == (Vector arg2) = arg1 == arg2
  (Atom arg1) == (Atom arg2) = arg1 == arg2
  (DottedList xs x) == (DottedList ys y) = xs == ys && x == y
  (List arg1) == (List arg2) = arg1 == arg2
  _ == _ = False

showVal :: LispVal -> String
showVal Unspecified = "<unspecified>"
showVal (Char c) = case c of
                     ' '       -> "#\\space"
                     '\n'      -> "#\\newline"
                     otherwise -> show c
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Vector elements) = "#(" ++ unwordsList (elems elements) ++ ")"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
     (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               deriving (Eq)

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

