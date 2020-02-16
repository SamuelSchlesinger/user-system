module UserSystem.Server.Reaper where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.STM
import UserSystem.Database
import Control.Exception
import Control.Monad.Reader

data SessionReaper = SessionReaper ThreadId (TVar SessionReaperState)

data SessionReaperState = Running | Stopping | Stopped
  deriving Eq

startSessionReaper :: DatabaseT IO SessionReaper
startSessionReaper = DatabaseT $ ReaderT \pool -> uninterruptibleMask \restore -> do
  stateRef <- newTVarIO Running
  let
    go = do
      handle (\(_ :: SomeException) -> return ()) (restore $ (runReaderT . unDatabaseT) (reapSessions' stateRef) pool)
      test <- atomically $ do
        readTVar stateRef >>= \case
          Stopping -> do
            writeTVar stateRef Stopped
            return True
          Running -> return False
          Stopped -> error "impossible"
      if test then return () else go
  pid <- forkIO go
  return $ SessionReaper pid stateRef
  where
    reapSessions' stateRef = do
      reapSessions
      void $ liftIO (race (threadDelay 10000000) (atomically $ check =<< ((== Stopping) <$> readTVar stateRef)))

stopSessionReaper :: SessionReaper -> IO ()
stopSessionReaper (SessionReaper pid stateRef) = do
  atomically $ writeTVar stateRef Stopping
  atomically $ check =<< ((== Stopped) <$> readTVar stateRef)
  killThread pid
