module Main where

import Control.Monad.IO.Class
import Data.Maybe
import System.Console.Haskeline
import System.Environment

import Scheme

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

replLoop :: InputT IO ()
replLoop = do
    env <- liftIO newEnv
    until_ quitPred (getInputLine "Lisp>>> ") (evalAndPrint env . fromJust)
  where
    quitPred Nothing = True
    quitPred (Just "quit") = True
    quitPred _ = False

    evalAndPrint env expr = do
      evalResult <- liftIO $ evalString env expr
      outputStrLn $ either show show evalResult

runRepl :: IO ()
runRepl = runInputT defaultSettings replLoop

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

