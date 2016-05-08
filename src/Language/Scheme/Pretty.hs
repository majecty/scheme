module Language.Scheme.Pretty
  ( prettyPrint
  ) where

import Text.PrettyPrint

import Language.Scheme.Types

prettyPrint :: Pretty a => a -> String
prettyPrint = render . pretty

class Pretty a where
  pretty :: a -> Doc

instance Pretty a => Pretty [a] where
  pretty as = vcat $ fmap pretty as

instance Pretty SExpr where
  pretty val@(SAtom _) = text $ show val
  pretty val@(SBool _) = text $ show val
  pretty val@(SChar _) = text $ show val
  pretty val@(SNumber _) = text $ show val
  pretty val@(SString _) = text $ show val
  pretty form@(SDottedList xs x) = text $ show form
  pretty form@(SVector arr) = text $ show form
  pretty (SList [atom@(SAtom x)]) = char '(' <> pretty atom <> char ')'
  pretty (SList [atom@(SAtom "quote"), x]) = text "'" <> pretty x
  pretty (SList []) = text "()"
  pretty (SList (x:xs)) =
    char '(' <> pretty x <+> nest (length (show x)) (prettyList xs <> char ')')
  pretty _ = empty

prettyList :: [SExpr] -> Doc
prettyList xs = sep $ map pretty xs
