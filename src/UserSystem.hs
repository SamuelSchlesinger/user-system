module UserSystem where

import Control.Exception 
  ( finally )
import Control.Monad.Reader 
  ( ask
  , liftIO
  , ReaderT(runReaderT) )
import Data.IORef
  ( newIORef
  , readIORef
  , atomicWriteIORef )
import Data.Time.Clock 
  ( getCurrentTime )
import Network.Wai.Handler.Warp
  ( setOnException
  , setPort
  , defaultSettings
  , runSettings )
import Network.Wai.Middleware.RequestLogger
  ( logStdoutDev )
import Servant.Server 
  ( serveWithContext
  , hoistServerWithContext )
import System.Directory
  ( setCurrentDirectory )
import System.Environment
  ( getEnv )
import System.IO 
  ( hPutStr
  , stderr )
import System.Posix.User
  ( getEffectiveUserName )
import UserSystem.Database
  ( testInfo
  , runDatabaseT
  , DatabaseT(unDatabaseT) )
import UserSystem.Server
  ( userSystemProxy
  , ctx
  , ctxProxy
  , server
  )
import UserSystem.Server.Reaper
  ( stopSessionReaper
  , startSessionReaper
  )

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
                userSystemProxy 
                (ctx pool) 
                (hoistServerWithContext userSystemProxy ctxProxy
                   (flip (runReaderT . unDatabaseT) pool) 
                   server)
    let settings = setOnException exceptionPrinter $ setPort 8080 defaultSettings
    liftIO (runSettings settings $ logStdoutDev app)

exceptionPrinter :: (Show a, Show b) => a -> b -> IO ()
exceptionPrinter _ ex = do
  now <- getCurrentTime
  hPutStr stderr $ "exception (" <> show now <> "): "
  print ex
