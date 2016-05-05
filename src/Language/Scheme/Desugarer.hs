module Language.Scheme.Desugarer
  ( desugar
  ) where

import Control.Monad.Except

import Language.Scheme.Types

desugar :: LispVal -> ThrowsError LispVal
desugar expr@(List (Atom "let": _)) = desugarLet expr
desugar expr = return expr

desugarLet :: LispVal -> ThrowsError LispVal
desugarLet form@(List (Atom "let" : (List bindings) : body)) = do
  (params, args) <- unzip <$> traverse extract bindings
  let lambda = List (Atom "lambda" : (List params) : body)
  return $ List (lambda : args)
  where
    extract :: LispVal -> ThrowsError (LispVal, LispVal)
    extract (List [id@(Atom _), valExpr]) = return (id, valExpr)
    extract _ = throwError $ BadSpecialForm "Malformed let" form
desugarLet badForm = throwError $ BadSpecialForm "Malformed let" badForm
