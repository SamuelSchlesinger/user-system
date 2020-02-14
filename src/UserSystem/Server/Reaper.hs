module UserSystem.Server.Reaper where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.STM
import UserSystem.Database
import Control.Exception
import Control.Monad.Reader

data SessionReaper = SessionReaper ThreadId (TVar Bool) (TVar Bool)

startSessionReaper :: DatabaseT IO SessionReaper
startSessionReaper = DatabaseT $ ReaderT \pool -> uninterruptibleMask \restore -> do
  stopRef <- newTVarIO False
  finishedRef <- newTVarIO False
  let
    go = do
      handle (\(_ :: SomeException) -> return ()) (restore $ (runReaderT . unDatabaseT) (reapSessions' stopRef) pool)
      test <- atomically $ do
        stopNow <- readTVar stopRef
        if stopNow then do
          writeTVar finishedRef True
          return True
        else
          return False
      if test then return () else go
  pid <- forkIO go
  return $ SessionReaper pid stopRef finishedRef
  where
    reapSessions' stopRef = do
      reapSessions
      void $ liftIO (race (threadDelay 10000000) (atomically $ check =<< readTVar stopRef))

stopSessionReaper :: SessionReaper -> IO ()
stopSessionReaper (SessionReaper pid stop finished) = do
  atomically $ writeTVar stop True
  atomically $ check =<< readTVar finished
  killThread pid


