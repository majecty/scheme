module Main where

import qualified Data.Array.IArray as IArray
import Data.Either
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Language.Scheme
import Language.Scheme.Desugarer
import Language.Scheme.Parser

evalSpec :: Spec
evalSpec =
  describe "evalString" $ do
    it "evaluates a char" $ do
      env <- newEnv
      evalString env "#\\space" `shouldReturn` (Right $ Char ' ')
      evalString env "#\\newline" `shouldReturn` (Right $ Char '\n')
      evalString env "#\\a" `shouldReturn` (Right $ Char 'a')
      evalString env "#\\A" `shouldReturn` (Right $ Char 'A')
      evalString env "#\\(" `shouldReturn` (Right $ Char '(')
      evalString env "#\\1" `shouldReturn` (Right $ Char '1')

    it "evaluates a string" $ do
      env <- newEnv
      evalString env "\"foo\"" `shouldReturn` (Right $ String "foo")

    it "evaluates a positive number" $ do
      env <- newEnv
      evalString env "123" `shouldReturn` (Right $ Number 123)

    it "evaluates a negative number" $ do
      env <- newEnv
      evalString env "-123" `shouldReturn` (Right . Number . negate $ 123)

    it "evaluates an atom" $ do
      env <- newEnv
      evalString env "'foo" `shouldReturn` (Right $ Atom "foo")

    it "evaluates a bool value" $ do
      env <- newEnv
      evalString env "#t"  `shouldReturn` (Right $ Bool True)
      evalString env "#f"  `shouldReturn` (Right $ Bool False)
      evalString env "'#f" `shouldReturn` (Right $ Bool False)

    it "evaluates a vector" $ do
      env <- newEnv
      evalString env "#()"  `shouldReturn` (Right $ Vector $ IArray.listArray (0, -1) [])
      evalString env "#(1 'foo \"bar\")"  `shouldReturn`
        (Right $ Vector $ IArray.listArray (0, 2) [Number 1, List [Atom "quote", Atom "foo"], String "bar"])

    it "evaluates begin expressions sequentially from left to right" $ do
      env <- newEnv
      evalString env "(begin (define x 0) (set! x 5) (+ x 1))"
        `shouldReturn` (Right $ Number 6)

    it "defines a variable" $ do
      env <- newEnv
      evalString env "(define x 28)" `shouldReturn` (Right $ Number 28)
      evalString env "x" `shouldReturn` (Right $ Number 28)

    it "provides type predicates" $ do
      env <- newEnv
      evalString env "(boolean? #f)" `shouldReturn` (Right $ Bool True)
      evalString env "(boolean? 0)" `shouldReturn` (Right $ Bool False)
      evalString env "(boolean? '())" `shouldReturn` (Right $ Bool False)

      evalString env "(pair? '(a . b))" `shouldReturn` (Right $ Bool True)
      evalString env "(pair? '(a b c))" `shouldReturn` (Right $ Bool True)
      evalString env "(pair? '())" `shouldReturn` (Right $ Bool False)

      evalString env "(list? '(a b c))" `shouldReturn` (Right $ Bool True)
      evalString env "(list? '())" `shouldReturn` (Right $ Bool True)
      evalString env "(list? '(a . b))" `shouldReturn` (Right $ Bool False)

      evalString env "(symbol? 'foo)" `shouldReturn` (Right $ Bool True)
      evalString env "(symbol? (car '(a b)))" `shouldReturn` (Right $ Bool True)
      evalString env "(symbol? \"bar\")" `shouldReturn` (Right $ Bool False)
      evalString env "(symbol? 'nil)" `shouldReturn` (Right $ Bool True)
      evalString env "(symbol? '())" `shouldReturn` (Right $ Bool False)
      evalString env "(symbol? #f)" `shouldReturn` (Right $ Bool False)

      evalString env "(number? 123)" `shouldReturn` (Right $ Bool True)
      evalString env "(number? -123)" `shouldReturn` (Right $ Bool True)

      evalString env "(char? #\\a)" `shouldReturn` (Right $ Bool True)
      evalString env "(char? #\\newline)" `shouldReturn` (Right $ Bool True)
      evalString env "(char? 'foo)" `shouldReturn` (Right $ Bool False)
      evalString env "(char? \"bar\")" `shouldReturn` (Right $ Bool False)
      evalString env "(char? 'nil)" `shouldReturn` (Right $ Bool False)
      evalString env "(char? '())" `shouldReturn` (Right $ Bool False)
      evalString env "(char? #f)" `shouldReturn` (Right $ Bool False)

      evalString env "(string? 'foo)" `shouldReturn` (Right $ Bool False)
      evalString env "(string? \"bar\")" `shouldReturn` (Right $ Bool True)
      evalString env "(string? 'nil)" `shouldReturn` (Right $ Bool False)
      evalString env "(string? '())" `shouldReturn` (Right $ Bool False)
      evalString env "(string? #f)" `shouldReturn` (Right $ Bool False)

      evalString env "(vector? '#())" `shouldReturn` (Right $ Bool True)
      evalString env "(vector? '#(1 'foo))" `shouldReturn` (Right $ Bool True)
      evalString env "(vector? 'foo)" `shouldReturn` (Right $ Bool False)
      evalString env "(vector? \"bar\")" `shouldReturn` (Right $ Bool False)
      evalString env "(vector? '())" `shouldReturn` (Right $ Bool False)
      evalString env "(vector? #f)" `shouldReturn` (Right $ Bool False)

      evalString env "(procedure? car)" `shouldReturn` (Right $ Bool True)
      evalString env "(procedure? 'car)" `shouldReturn` (Right $ Bool False)
      evalString env "(procedure? (lambda (x) (* x x)))" `shouldReturn` (Right $ Bool True)
      evalString env "(procedure? '(lambda (x) (* x x)))" `shouldReturn` (Right $ Bool False)

    it "implements char comparison procedures" $ do
      env <- newEnv
      evalString env "(char=? #\\a #\\a)" `shouldReturn` (Right $ Bool True)
      evalString env "(char=? #\\A #\\A)" `shouldReturn` (Right $ Bool True)
      evalString env "(char=? #\\1 #\\1)" `shouldReturn` (Right $ Bool True)

      evalString env "(char<? #\\a #\\a)" `shouldReturn` (Right $ Bool False)
      evalString env "(char<? #\\a #\\b)" `shouldReturn` (Right $ Bool True)
      evalString env "(char<? #\\A #\\B)" `shouldReturn` (Right $ Bool True)
      evalString env "(char<? #\\1 #\\2)" `shouldReturn` (Right $ Bool True)

      evalString env "(char>? #\\a #\\a)" `shouldReturn` (Right $ Bool False)
      evalString env "(char>? #\\a #\\b)" `shouldReturn` (Right $ Bool False)
      evalString env "(char>? #\\A #\\B)" `shouldReturn` (Right $ Bool False)
      evalString env "(char>? #\\1 #\\2)" `shouldReturn` (Right $ Bool False)

      evalString env "(char<=? #\\a #\\a)" `shouldReturn` (Right $ Bool True)
      evalString env "(char<=? #\\a #\\b)" `shouldReturn` (Right $ Bool True)
      evalString env "(char<=? #\\A #\\B)" `shouldReturn` (Right $ Bool True)
      evalString env "(char<=? #\\1 #\\2)" `shouldReturn` (Right $ Bool True)

      evalString env "(char>=? #\\a #\\a)" `shouldReturn` (Right $ Bool True)
      evalString env "(char>=? #\\a #\\b)" `shouldReturn` (Right $ Bool False)
      evalString env "(char>=? #\\A #\\B)" `shouldReturn` (Right $ Bool False)
      evalString env "(char>=? #\\1 #\\2)" `shouldReturn` (Right $ Bool False)

    it "implements string comparison procedures" $ do
      env <- newEnv
      evalString env "(string=? \"foo\" \"foo\")" `shouldReturn` (Right $ Bool True)
      evalString env "(string<? \"bar\" \"foo\")" `shouldReturn` (Right $ Bool True)
      evalString env "(string>? \"bar\" \"foo\")" `shouldReturn` (Right $ Bool False)
      evalString env "(string<? \"foo\" \"foz\")" `shouldReturn` (Right $ Bool True)
      evalString env "(string>? \"foo\" \"foz\")" `shouldReturn` (Right $ Bool False)
      evalString env "(string<=? \"bar\" \"foo\")" `shouldReturn` (Right $ Bool True)
      evalString env "(string>=? \"bar\" \"foo\")" `shouldReturn` (Right $ Bool False)
      evalString env "(string<=? \"foo\" \"foz\")" `shouldReturn` (Right $ Bool True)
      evalString env "(string>=? \"foo\" \"foz\")" `shouldReturn` (Right $ Bool False)
      evalString env "(string<=? \"foo\" \"foo\")" `shouldReturn` (Right $ Bool True)
      evalString env "(string>=? \"foo\" \"foo\")" `shouldReturn` (Right $ Bool True)

    it "implements string-length procedures" $ do
      env <- newEnv
      evalString env "(string-length \"\")" `shouldReturn` (Right $ Number 0)
      evalString env "(string-length \"foo\")" `shouldReturn` (Right $ Number 3)
      evalString env "(string-length 'foo)" `shouldReturn` (Left $ TypeMismatch "string" (Atom "foo"))

    it "implements list<->vector conversion procedures" $ do
      env <- newEnv
      evalString env "(list->vector '(1 2 3))" `shouldReturn`
        (Right $ Vector $ IArray.listArray (0, 2) [Number 1, Number 2, Number 3])
      evalString env "(vector->list #(1 2 3))" `shouldReturn`
        (Right $ List [Number 1, Number 2, Number 3])

    it "implements symbol<->string conversion procedures" $ do
      env <- newEnv
      evalString env "(symbol->string 'foo)" `shouldReturn` (Right $ String "foo")
      evalString env "(string->symbol \"foo\")" `shouldReturn` (Right $ Atom "foo")

    it "implements symbol->string as an inverse of string->symbol" $ property $
      \s -> monadicIO $ do
        pre $ not $ null s
        env  <- run $ newEnv
        let p1 = List [Atom "symbol->string", List [Atom "quote", Atom s]]
        (Right res1) <- run $ evalLispVal env p1
        let p2 = List [Atom "string->symbol", res1]
        (Right res2) <- run $ evalLispVal env p2
        assert $ res2 == Atom s

    it "implements char<->integer conversion procedures" $ do
      env <- newEnv
      evalString env "(char->integer #\\a)" `shouldReturn` (Right $ Number 97)
      evalString env "(integer->char 97)" `shouldReturn` (Right $ Char 'a')

    it "implements char->integer as an inverse of integer->char" $ property $
      \c -> monadicIO $ do
        env  <- run $ newEnv
        let p1 = List [Atom "char->integer", Char c]
        (Right res1) <- run $ evalLispVal env p1
        let p2 = List [Atom "integer->char", res1]
        (Right res2) <- run $ evalLispVal env p2
        assert $ res2 == Char c

    it "implements char predicate procedures" $ do
      env <- newEnv
      evalString env "(char-alphabetic? #\\a)" `shouldReturn` (Right $ Bool True)
      evalString env "(char-alphabetic? #\\A)" `shouldReturn` (Right $ Bool True)
      evalString env "(char-alphabetic? #\\1)" `shouldReturn` (Right $ Bool False)

      evalString env "(char-numeric? #\\a)" `shouldReturn` (Right $ Bool False)
      evalString env "(char-numeric? #\\A)" `shouldReturn` (Right $ Bool False)
      evalString env "(char-numeric? #\\1)" `shouldReturn` (Right $ Bool True)

      evalString env "(char-whitespace? #\\a)" `shouldReturn` (Right $ Bool False)
      evalString env "(char-whitespace? #\\A)" `shouldReturn` (Right $ Bool False)
      evalString env "(char-whitespace? #\\space)" `shouldReturn` (Right $ Bool True)
      evalString env "(char-whitespace? #\\newline)" `shouldReturn` (Right $ Bool True)

      evalString env "(char-upper-case? #\\a)" `shouldReturn` (Right $ Bool False)
      evalString env "(char-upper-case? #\\A)" `shouldReturn` (Right $ Bool True)
      evalString env "(char-upper-case? #\\1)" `shouldReturn` (Right $ Bool False)

      evalString env "(char-lower-case? #\\a)" `shouldReturn` (Right $ Bool True)
      evalString env "(char-lower-case? #\\A)" `shouldReturn` (Right $ Bool False)
      evalString env "(char-lower-case? #\\1)" `shouldReturn` (Right $ Bool False)

    it "implements char-upcase/char-downcase procedures" $ do
      env <- newEnv
      evalString env "(char-upcase #\\a)" `shouldReturn` (Right $ Char 'A')
      evalString env "(char-upcase #\\A)" `shouldReturn` (Right $ Char 'A')
      evalString env "(char-upcase #\\1)" `shouldReturn` (Right $ Char '1')
      evalString env "(char-downcase #\\A)" `shouldReturn` (Right $ Char 'a')
      evalString env "(char-downcase #\\a)" `shouldReturn` (Right $ Char 'a')
      evalString env "(char-downcase #\\1)" `shouldReturn` (Right $ Char '1')

desugarSpec :: Spec
desugarSpec =
  describe "desugarer" $ do
    let readAndDesugar str = readExpr str >>= desugar

    it "desugars let" $ do
      let infix 1 `shouldDesugaredInto`
          sugared `shouldDesugaredInto` desugared =
              (readAndDesugar sugared) `shouldBe` (readExpr desugared)

      "(let ((x 1)) x)" `shouldDesugaredInto` "((lambda (x) x) 1)"
      "(let ((x 1) (y 2)) (+ x y)))" `shouldDesugaredInto` "((lambda (x y) (+ x y)) 1 2)"
      "(let ((x 1)) (print x) (+ x 1)))"
          `shouldDesugaredInto` "((lambda (x) (print x) (+ x 1)) 1)"

    it "throws on malformed let bindings" $ do
      let shouldFailToDesugar str = readAndDesugar str `shouldSatisfy` isLeft
      shouldFailToDesugar "(let x x)"
      shouldFailToDesugar "(let (x) x)"
      shouldFailToDesugar "(let ((1 2)) 1)"
      shouldFailToDesugar "(let ((x)) x)"
      shouldFailToDesugar "(let ((x 1 2) x)"

    it "does not desugar" $ do
      let shouldNotDesugar sugared =
              (readAndDesugar sugared) `shouldBe` (readExpr sugared)
      shouldNotDesugar "(lambda (x y) (some x y))"
      shouldNotDesugar "(define x 28)"
      shouldNotDesugar "#t"
      shouldNotDesugar "()"

main = hspec $ do
  evalSpec
  desugarSpec
