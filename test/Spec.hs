module Main where

import Scheme
import Test.Hspec

evalSpec :: Spec
evalSpec =
  describe "evalString" $ do
    it "evaluates a string" $ do
      env <- primitiveBindings
      res <- evalString env "\"foo\""
      res `shouldBe` (Right $ String "foo")

    it "evaluates a positive number" $ do
      env <- primitiveBindings
      res <- evalString env "123"
      res `shouldBe` (Right $ Number 123)

    it "evaluates a negative number" $ do
      env <- primitiveBindings
      res <- evalString env "-123"
      res `shouldBe` (Right $ Number . negate $ 123)

    it "evaluates an atom" $ do
      env <- primitiveBindings
      res <- evalString env "'foo"
      res `shouldBe` (Right $ Atom "foo")

    it "evaluates a true value" $ do
      env <- primitiveBindings
      res <- evalString env "#t"
      res `shouldBe` (Right $ Bool True)

    it "evaluates a false value" $ do
      env <- primitiveBindings
      res <- evalString env "#f"
      res `shouldBe` (Right $ Bool False)

    it "evaluates begin expressions sequentially from left to right" $ do
      env <- primitiveBindings
      res <- evalString env "(begin (define x 0) (set! x 5) (+ x 1))"
      res `shouldBe` (Right $ Number 6)

main = hspec $ do
  evalSpec
