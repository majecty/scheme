module Main where

import Scheme
import Test.Hspec

evalSpec :: Spec
evalSpec =
  describe "evalString" $ do
    it "evaluates a string" $ do
      env <- primitiveBindings
      evalString env "\"foo\"" `shouldReturn` (Right $ String "foo")

    it "evaluates a positive number" $ do
      env <- primitiveBindings
      evalString env "123" `shouldReturn` (Right $ Number 123)

    it "evaluates a negative number" $ do
      env <- primitiveBindings
      evalString env "-123" `shouldReturn` (Right $ Number . negate $ 123)

    it "evaluates an atom" $ do
      env <- primitiveBindings
      evalString env "'foo" `shouldReturn` (Right $ Atom "foo")

    it "evaluates a true value" $ do
      env <- primitiveBindings
      evalString env "#t" `shouldReturn` (Right $ Bool True)

    it "evaluates a false value" $ do
      env <- primitiveBindings
      evalString env "#f" `shouldReturn` (Right $ Bool False)

    it "evaluates begin expressions sequentially from left to right" $ do
      env <- primitiveBindings
      evalString env "(begin (define x 0) (set! x 5) (+ x 1))"
        `shouldReturn` (Right $ Number 6)

main = hspec $ do
  evalSpec
