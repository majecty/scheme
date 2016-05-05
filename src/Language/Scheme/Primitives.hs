{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
module Language.Scheme.Primitives
  ( ioPrimitives
  , primitives
  ) where

import Control.Monad.Except
import qualified Data.Array.IArray as IArray
import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI
import Data.Char ( toUpper, toLower, isAlpha, isDigit
                 , isUpper, isLower, isSpace, ord, chr )
import System.IO

import Language.Scheme.Parser
import Language.Scheme.Types

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinopCI :: CI.FoldCase a => (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinopCI unpacker op args = if length args /= 2
                                 then throwError $ NumArgs 2 args
                                 else do left <- unpacker $ args !! 0
                                         right <- unpacker $ args !! 1
                                         return $ Bool $ (CI.foldCase left) `op` (CI.foldCase right)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

charBoolBinopCI :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charBoolBinopCI = boolBinopCI unpackChar

charBoolBinop :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charBoolBinop = boolBinop unpackChar

strBoolBinopCI :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinopCI = boolBinopCI unpackStr

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum = \case
  (Number n) -> return n
  (String n) -> let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
  (List [n]) -> unpackNum n
  notNum     -> throwError $ TypeMismatch "number" notNum

unpackChar :: LispVal -> ThrowsError Char
unpackChar = \case
  (Char c)  -> return c
  notChar   -> throwError $ TypeMismatch "char" notChar

unpackStr :: LispVal -> ThrowsError String
unpackStr = \case
  (String s)  -> return s
  (Number s)  -> return $ show s
  (Bool s)    -> return $ show s
  notString   -> throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool = \case
  (Bool b)  -> return b
  notBool   -> throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

car :: [LispVal] -> ThrowsError LispVal
car = \case
  [List (x : xs)]         -> return x
  [DottedList (x : xs) _] -> return x
  [badArg]                -> throwError $ TypeMismatch "pair" badArg
  badArgList              -> throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr = \case
  [List (x : xs)]         -> return $ List xs
  [DottedList (_ : xs) x] -> return $ DottedList xs x
  [DottedList [xs] x]     -> return x
  [badArg]                -> throwError $ TypeMismatch "pair" badArg
  badArgList              -> throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons = \case
  [x1, List []]             -> return $ List [x1]
  [x, List xs]              -> return $ List $ x : xs
  [x, DottedList xs xlast]  -> return $ DottedList (x : xs) xlast
  [x1, x2]                  -> return $ DottedList [x1] x2
  badArgList                -> throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal = \case
  [arg1, arg2] -> do
    primitiveEquals <- fmap or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
  badArgList -> throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv = \case
  [arg1, arg2]  -> return . Bool $ arg1 == arg2
  badArgList    -> throwError $ NumArgs 2 badArgList

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean = \case
  [(Bool _)]  -> return . Bool $ True
  [_]         -> return . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isPair :: [LispVal] -> ThrowsError LispVal
isPair = \case
  [List (x:y:_)]    -> return . Bool $ True
  [DottedList _ _]  -> return . Bool $ True
  [_]               -> return . Bool $ False
  badArgList        -> throwError $ NumArgs 1 badArgList

isList :: [LispVal] -> ThrowsError LispVal
isList = \case
  [(List _)]  -> return . Bool $ True
  [_]         -> return . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol = \case
  [Atom _]   -> return . Bool $ True
  [_]        -> return . Bool $ False
  badArgList -> throwError $ NumArgs 1 badArgList

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString = \case
  [Atom s]    -> return . String $ s
  [badArg]    -> throwError $ TypeMismatch "symbol" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol = \case
  [String s]  -> return . Atom $ s
  [badArg]    -> throwError $ TypeMismatch "string" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber = \case
  [Number _]  -> return . Bool $ True
  [_]         -> return . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isChar :: [LispVal] -> ThrowsError LispVal
isChar = \case
  [Char _]    -> return . Bool $ True
  [_]         -> return . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isString :: [LispVal] -> ThrowsError LispVal
isString = \case
  [String _]  -> return . Bool $ True
  [_]         -> return . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isVector :: [LispVal] -> ThrowsError LispVal
isVector = \case
  [Vector _]  -> return . Bool $ True
  [_]         -> return . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

listToVector :: [LispVal] -> ThrowsError LispVal
listToVector = \case
  [List xs]   -> return . Vector $ IArray.listArray (0, length xs - 1) xs
  [badArg]    -> throwError $ TypeMismatch "list" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

vectorToList :: [LispVal] -> ThrowsError LispVal
vectorToList = \case
  [Vector vs] -> return . List $ IArray.elems vs
  [badArg]    -> throwError $ TypeMismatch "vector" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

isPort :: [LispVal] -> ThrowsError LispVal
isPort = \case
  [Port _]    -> return . Bool $ True
  [_]         -> return . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure = \case
  [PrimitiveFunc _] -> return . Bool $ True
  [IOFunc _]        -> return . Bool $ True
  [Func _ _ _ _]    -> return . Bool $ True
  [_]               -> return . Bool $ False
  badArgList        -> throwError $ NumArgs 1 badArgList

charIsAlphabetic :: [LispVal] -> ThrowsError LispVal
charIsAlphabetic = \case
  [Char c]    -> return . Bool $ isAlpha c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charIsNumeric :: [LispVal] -> ThrowsError LispVal
charIsNumeric = \case
  [Char c]    -> return . Bool $ isDigit c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charIsWhitespace :: [LispVal] -> ThrowsError LispVal
charIsWhitespace = \case
  [Char c]    -> return . Bool $ isSpace c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charIsUpperCase :: [LispVal] -> ThrowsError LispVal
charIsUpperCase = \case
  [Char c]    -> return . Bool $ isUpper c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charIsLowerCase :: [LispVal] -> ThrowsError LispVal
charIsLowerCase = \case
  [Char c]    -> return . Bool $ isLower c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charToInteger :: [LispVal] -> ThrowsError LispVal
charToInteger = \case
  [Char c]    -> return . Number . fromIntegral . ord $ c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

integerToChar :: [LispVal] -> ThrowsError LispVal
integerToChar = \case
  [Number c]  -> return . Char . chr . fromIntegral $ c
  [badArg]    -> throwError $ TypeMismatch "number" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charUpcase :: [LispVal] -> ThrowsError LispVal
charUpcase = \case
  [Char c]    -> return . Char $ toUpper c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charDowncase :: [LispVal] -> ThrowsError LispVal
charDowncase = \case
  [Char c]    -> return . Char $ toLower c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength = \case
  [String s]  -> return . Number $ fromIntegral . length $ s
  [badArg]    -> throwError $ TypeMismatch "string" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-ci=?", strBoolBinopCI (==)),
              ("string-ci<?", strBoolBinopCI (<)),
              ("string-ci>?", strBoolBinopCI (>)),
              ("string-ci<=?", strBoolBinopCI (<=)),
              ("string-ci>=?", strBoolBinopCI (>=)),
              ("string-length", stringLength),
              ("char=?", charBoolBinop (==)),
              ("char<?", charBoolBinop (<)),
              ("char>?", charBoolBinop (>)),
              ("char<=?", charBoolBinop (<=)),
              ("char>=?", charBoolBinop (>=)),
              ("char-ci=?", charBoolBinopCI (==)),
              ("char-ci<?", charBoolBinopCI (<)),
              ("char-ci>?", charBoolBinopCI (>)),
              ("char-ci<=?", charBoolBinopCI (<=)),
              ("char-ci>=?", charBoolBinopCI (>=)),
              ("char-alphabetic?", charIsAlphabetic),
              ("char-numeric?", charIsNumeric),
              ("char-whitespace?", charIsWhitespace),
              ("char-upper-case?", charIsUpperCase),
              ("char-lower-case?", charIsLowerCase),
              ("char->integer", charToInteger),
              ("integer->char", integerToChar),
              ("char-upcase", charUpcase),
              ("char-downcase", charDowncase),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("boolean?", isBoolean),
              ("pair?", isPair),
              ("list?", isList),
              ("symbol?", isSymbol),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol),
              ("number?", isNumber),
              ("char?", isChar),
              ("string?", isString),
              ("vector?", isVector),
              ("vector->list", vectorToList),
              ("list->vector", listToVector),
              ("port?", isPort),
              ("procedure?", isProcedure)]

-- FIXME: Add more IO primitives
ioPrimitives :: [(String, [LispVal] -> EvalM LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("display", display)]

makePort :: IOMode -> [LispVal] -> EvalM LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> EvalM LispVal
closePort = \case
  [Port port] -> liftIO $ hClose port >> (return $ Bool True)
  _           -> return $ Bool False

readProc :: [LispVal] -> EvalM LispVal
readProc = \case
  []          -> readProc [Port stdin]
  [Port port] -> (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

writeProc :: [LispVal] -> EvalM LispVal
writeProc = \case
  [obj]             -> writeProc [obj, Port stdout]
  [obj, Port port]  -> liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> EvalM LispVal
readContents = \case
  [String filename] -> fmap String $ liftIO $ readFile filename

display :: [LispVal] -> EvalM LispVal
display = \case
  [val]       -> liftIO $ putStr (show val) >> return Unspecified
  badArgList  -> throwError $ NumArgs 1 badArgList

