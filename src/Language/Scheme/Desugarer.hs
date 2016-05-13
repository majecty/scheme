module Language.Scheme.Desugarer
  ( desugar
  ) where

import Control.Monad.Except

import Language.Scheme.Types

desugar :: SExpr -> ThrowsError SExpr
desugar expr@(SList (SAtom "let": _)) = desugarLet expr
desugar expr = pure expr

desugarLet :: SExpr -> ThrowsError SExpr
desugarLet form@(SList (SAtom "let" : SList bindings : body)) = do
  (params, args) <- unzip <$> traverse extract bindings
  let lambda = SList (SAtom "lambda" : SList params : body)
  pure $ SList (lambda : args)
  where
    extract :: SExpr -> ThrowsError (SExpr, SExpr)
    extract (SList [id@(SAtom _), valExpr]) = pure (id, valExpr)
    extract _ = throwError $ BadSpecialForm "Malformed let" form
desugarLet badForm = throwError $ BadSpecialForm "Malformed let" badForm
