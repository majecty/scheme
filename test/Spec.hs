module Main where

import Scheme
import Test.Hspec

infix 1 `shouldReturnRight`

action `shouldReturnRight` expected = action >>= (`shouldBe` (Right expected))

evalSpec :: Spec
evalSpec =
  describe "evalString" $ do
    it "evaluates a string" $ do
      env <- newEnv
      evalString env "\"foo\"" `shouldReturnRight` String "foo"

    it "evaluates a positive number" $ do
      env <- newEnv
      evalString env "123" `shouldReturnRight` Number 123

    it "evaluates a negative number" $ do
      env <- newEnv
      evalString env "-123" `shouldReturnRight` (Number . negate $ 123)

    it "evaluates an atom" $ do
      env <- newEnv
      evalString env "'foo" `shouldReturnRight` Atom "foo"

    it "evaluates a true value" $ do
      env <- newEnv
      evalString env "#t" `shouldReturnRight` Bool True

    it "evaluates a false value" $ do
      env <- newEnv
      evalString env "#f" `shouldReturnRight` Bool False

    it "evaluates begin expressions sequentially from left to right" $ do
      env <- newEnv
      evalString env "(begin (define x 0) (set! x 5) (+ x 1))"
        `shouldReturnRight` Number 6

    it "defines a variable" $ do
      env <- newEnv
      evalString env "(define x 28)" `shouldReturnRight` Number 28
      evalString env "x" `shouldReturnRight` Number 28

main = hspec $ do
  evalSpec
