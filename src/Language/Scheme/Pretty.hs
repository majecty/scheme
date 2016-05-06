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

instance Pretty LispVal where
  pretty val@(Atom _) = text $ show val
  pretty val@(Bool _) = text $ show val
  pretty val@(Char _) = text $ show val
  pretty val@(Number _) = text $ show val
  pretty val@(String _) = text $ show val
  pretty form@(DottedList xs x) = text $ show form
  pretty form@(Vector arr) = text $ show form
  pretty (List [atom@(Atom x)]) = char '(' <> pretty atom <> char ')'
  pretty (List [atom@(Atom "quote"), x]) = text "'" <> pretty x
  pretty (List (atom@(Atom "quote"):xs)) = text "'(" <> prettyList xs <> char ')'
  pretty (List (x:xs)) =
    char '(' <> pretty x <+> nest (length (show x)) (prettyList xs <> char ')')
  pretty _ = empty

prettyList :: [LispVal] -> Doc
prettyList xs = sep $ map pretty xs
