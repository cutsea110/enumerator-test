{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module EnumTest where

import Control.Monad.Trans
import Control.Monad.State
import Data.Enumerator hiding (head)
import qualified Data.Enumerator.List as EL
import Data.List (genericSplitAt)

listFeeder :: (Monad m) => [a] -> Enumerator a m b
listFeeder = enumList 1

listFeederIO :: (Show a) => [a] -> Enumerator a IO b
listFeederIO xs (Continue k) | not (null xs) = do
  let (s1, s2) = genericSplitAt 1 xs
  liftIO $ putStr $ show s1 ++ " => "
  k (Chunks s1) >>== listFeederIO s2
listFeederIO _ step = returnI step

listFeederStateIO :: (Show a, Show s) => [a] -> Enumerator a (StateT s IO) ()
listFeederStateIO xs (Continue k) | not (null xs) = do
  n <- lift get
  let (s1, s2) = genericSplitAt 1 xs
  liftIO $ do
    putStr $ show s1
    putStr $ ": (" ++ show (head s1) ++ "," ++ show n ++ ") => "
  k (Chunks s1) >>== listFeederStateIO s2
listFeederStateIO _ s = do
  n <- lift get
  returnI s


printer :: Iteratee Integer IO ()
printer = do
  mx <- EL.head
  case mx of
    Nothing -> return ()
    Just x -> do
      tryIO $ print x
      printer

printerState :: Iteratee Integer (StateT Integer IO) ()
printerState = do
  mx <- EL.head
  case mx of
    Nothing -> return ()
    Just x -> do
      s <- lift get
      lift $ put (s*x)
      liftIO $ do
        putStr $ show x ++ " x " ++ show s
        putStrLn $ "=> " ++ show (x*s)
      printerState

instance MonadTrans (Iteratee el) where
  lift m = Iteratee $ m >>= \x -> return $ Yield x (Chunks [])
  
instance MonadIO m => MonadIO (Iteratee el m) where
  liftIO = lift . liftIO

test1 = run_ $ listFeeder [1..10] $$ printer

test2 = run_ $ listFeederIO [1..10] $$ printer

test3 = runStateT (run_ $ listFeederStateIO [1..10] $$ printerState) 1
