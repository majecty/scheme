module Scheme.Desugarer
  ( desugar
  ) where

import Scheme.Types

desugar :: LispVal -> LispVal
desugar = id
