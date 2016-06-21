{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import System.Console.Haskeline
import System.Directory
import System.FilePath
import System.Environment

import Language.Scheme

import ReplM

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then pure ()
     else action result >> until_ pred prompt action

schemeHistoryFile :: IO FilePath
schemeHistoryFile = do
  dataDir <- getAppUserDataDirectory "scheme"
  createDirectoryIfMissing True dataDir
  pure $ dataDir </> "scheme_history"

replLoop :: InputT ReplM ()
replLoop =
  until_ quitPred (getInputLine "Lisp>>> ") (evalAndPrint . fromJust)
  where
    quitPred Nothing = True
    quitPred (Just "quit") = True
    quitPred _ = False

    evalAndPrint str = do
      env <- lift ask
      evalResult <- liftIO $ evalString env str
      case evalResult of
        Left  e     -> (outputStrLn . show) e
        Right exprs -> traverse_ printAndBindResult exprs

    printAndBindResult expr = do
      name <- lift newName
      lift $ defineVar name expr
      outputStrLn $ name ++ " : " ++ (show expr)

runRepl :: IO ()
runRepl = do
  historyFile <- Just <$> schemeHistoryFile
  let settings = defaultSettings { historyFile }
  runReplM $ runInputT settings replLoop

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
