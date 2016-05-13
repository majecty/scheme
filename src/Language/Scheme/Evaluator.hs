{-# LANGUAGE FlexibleContexts #-}
module Language.Scheme.Evaluator
  ( evalLispVal
  , evalString
  , newEnv
  , runOne
  , withStandardLibrary
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import System.IO

import Paths_scheme

import Language.Scheme.Desugarer
import Language.Scheme.Env
import Language.Scheme.Primitives
import Language.Scheme.Reader
import Language.Scheme.Types

eval :: SExpr -> EvalM LispVal
eval val@(SString _) = pure $ sexprToLispVal val
eval val@(SChar _) = pure $ sexprToLispVal val
eval val@(SVector _) = pure $ sexprToLispVal val
eval val@(SNumber _) = pure $ sexprToLispVal val
eval val@(SBool b) = pure $ sexprToLispVal val
eval (SAtom id) = getVar id
eval (SList (SAtom "begin" : exps)) = last <$> traverse eval exps
eval (SList [SAtom "quote", val]) = pure $ sexprToLispVal val
eval (SList [SAtom "if", pred, conseq]) = do
    result <- eval pred
    case result of
      Bool False -> pure Unspecified
      _          -> eval conseq
eval form@(SList [SAtom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False  -> eval alt
        _           -> eval conseq
eval badForm@(SList (SAtom "if":_)) = throwError $ BadSpecialForm "Unrecognized special form" badForm
eval (SList [SAtom "set!", SAtom var, form]) =
    eval form >>= setVar var
eval (SList [SAtom "define", SAtom var, form]) =
    eval form >>= defineVar var
eval (SList (SAtom "define" : SList (SAtom var : params) : body)) =
    makeNormalFunc params body >>= defineVar var
eval (SList (SAtom "define" : SDottedList (SAtom var : params) varargs : body)) =
    makeVarargs varargs params body >>= defineVar var
eval (SList (SAtom "lambda" : SList params : body)) =
    makeNormalFunc params body
eval (SList (SAtom "lambda" : SDottedList params varargs : body)) =
    makeVarargs varargs params body
eval (SList (SAtom "lambda" : varargs@(SAtom _) : body)) =
    makeVarargs varargs [] body
eval (SList [SAtom "load", SString filename]) = do
    vals <- load filename
    last <$> traverse (desugar' >=> eval) vals
  where
    desugar' = liftThrows . desugar
eval (SList (function : args)) = do
    func <- eval function
    argVals <- traverse eval args
    apply func argVals
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> EvalM LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
       then throwError $ NumArgs (num params) args
       else do
           env <- liftIO $ bindVars closure $ zip params args
           env' <- bindVarArgs varargs env
           local (const env') evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody = last <$> traverse eval body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
              Nothing -> pure env

load :: String -> EvalM [SExpr]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

evalLispVal' :: Env -> SExpr -> IOThrowsError LispVal
evalLispVal' env expr =
  let desugar' = liftThrows . desugar
      evalResult = desugar' >=> eval $ expr :: EvalM LispVal
   in runEvalM env evalResult

evalString' :: Env -> String -> IOThrowsError [LispVal]
evalString' env str = case readExprList str of
                        Left e      -> throwError e
                        Right exprs -> traverse (evalLispVal' env) exprs

evalLispVal :: Env -> SExpr -> IO (Either LispError LispVal)
evalLispVal env = runExceptT . evalLispVal' env

evalString :: Env -> String -> IO (Either LispError [LispVal])
evalString env = runExceptT . evalString' env

runOne :: [String] -> IO ()
runOne args = do
    env <- newEnv >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    withStandardLibrary env $
      runExceptT (runEvalM env $ eval (SList [SAtom "load", SString (head args)]))
        >>= hPutStrLn stderr . either show show

withStandardLibrary :: (MonadIO m) => Env -> m () -> m ()
withStandardLibrary env action = do
    res <- liftIO $ loadStandardLibrary env
    case res of
      Left e  -> liftIO $ putStrLn $ "Error loading stdlib: " ++ show e
      Right _ -> action
  where loadStandardLibrary env = do
            stdlibPath <- getDataFileName "lib/stdlib.scm"
            evalLispVal env (SList [SAtom "load", SString stdlibPath])

newEnv :: IO Env
newEnv = primitiveBindings
  where
    makeFunc constructor (var, func) = (var, constructor func)

    primitiveBindings :: IO Env
    primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) builtinPrimitives
                                              ++ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)

runEvalM :: Env -> EvalM LispVal -> IOThrowsError LispVal
runEvalM env action = (runReaderT . run) action env

makeFunc :: Maybe String -> [SExpr] -> [SExpr] -> EvalM LispVal
makeFunc varargs params body = do
  env <- ask
  pure $ Func (map show params) varargs body env

makeNormalFunc :: [SExpr] -> [SExpr] -> EvalM LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: SExpr -> [SExpr] -> [SExpr] -> EvalM LispVal
makeVarargs = makeFunc . Just . show

applyProc :: [LispVal] -> EvalM LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

readAll :: [LispVal] -> EvalM LispVal
readAll [String filename] = sexprToLispVal . SList <$> load filename

builtinPrimitives :: [(String, [LispVal] -> EvalM LispVal)]
builtinPrimitives = [("apply", applyProc), ("read-all", readAll)]
