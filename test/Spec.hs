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

    it "evaluates a number" $ do
      env <- primitiveBindings
      res <- evalString env "123"
      res `shouldBe` (Right $ Number 123)

    it "evaluates an atom" $ do
      env <- primitiveBindings
      res <- evalString env "'foo"
      res `shouldBe` (Right $ Atom "foo")

    it "evaluates a boolean value" $ do
      env <- primitiveBindings
      res <- evalString env "#t"
      res `shouldBe` (Right $ Bool True)

main = hspec $ do
  evalSpec
