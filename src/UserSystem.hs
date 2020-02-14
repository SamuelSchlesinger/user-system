module UserSystem where

import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Data.Time.Clock
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant.Server
import System.Directory
import System.Environment
import System.IO
import System.Posix.User
import UserSystem.Database
import UserSystem.Server

main :: IO ()
main = do
  getEnv "USER_SYSTEM_LOCATION" >>= setCurrentDirectory
  connInfo <- testInfo <$> getEffectiveUserName
  sessionReaper <- newIORef Nothing
  flip finally (maybe (pure ()) stopSessionReaper =<< readIORef sessionReaper) $ runDatabaseT connInfo do
    pool <- ask
    reaper <- startSessionReaper
    liftIO $ atomicWriteIORef sessionReaper (Just reaper)
    let app = serveWithContext 
                freezeProxy 
                (ctx pool) 
                (hoistServerWithContext freezeProxy ctxProxy
                   (flip (runReaderT . unDatabaseT) pool) 
                   server)
    let settings = setOnException exceptionPrinter $ setPort 8080 defaultSettings
    liftIO (runSettings settings $ logStdoutDev app)

exceptionPrinter :: (Show a, Show b) => a -> b -> IO ()
exceptionPrinter _ ex = do
  now <- getCurrentTime
  hPutStr stderr $ "Exception @ " <> show now <> ": "
  print ex
