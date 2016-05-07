module Language.Scheme.Env
  ( bindVars
  , defineVar
  , getVar
  , nullEnv
  , setVar
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef

import Language.Scheme.Types

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= pure . maybe False (const True) . lookup var

getVar :: String -> EvalM LispVal
getVar var  =  do
    envRef <- ask
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: String -> LispVal -> EvalM LispVal
setVar var value = do
    envRef <- ask
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
    pure value


defineVar :: String -> LispVal -> EvalM LispVal
defineVar var value = do
    envRef <- ask
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar var value >> pure value
       else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          pure value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = fmap (++ env) (traverse addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       pure (var, ref)


