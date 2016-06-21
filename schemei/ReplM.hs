{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ReplM
  ( newName,
    ReplM,
    runReplM
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import System.Console.Haskeline

import Language.Scheme

type Count = Int
newtype ReplM a = ReplM {
    run :: (ReaderT Env (ReaderT (IORef Count) IO)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadException)

getCountRef :: ReplM (IORef Count)
getCountRef = ReplM . lift $ ask

getCount :: ReplM Count
getCount = do
  countRef <- getCountRef
  liftIO $ readIORef countRef

incCount :: ReplM ()
incCount = do
  count <- getCountRef
  liftIO $ modifyIORef count (+1)

newName :: ReplM String
newName = do
  incCount
  (("$"++) . show) <$> getCount

runReplM :: ReplM () -> IO ()
runReplM replM = do
  newEnv' <- liftIO newEnv
  loadStandardLibrary newEnv'
  initCount <- newIORef 0
  (flip runReaderT initCount . flip runReaderT newEnv' . run) replM

