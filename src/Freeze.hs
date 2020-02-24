module Freeze where

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
import Freeze.Database
  ( testInfo
  , runDatabaseT
  , DatabaseT(unDatabaseT) )
import Freeze.Server
  ( freezeProxy
  , ctx
  , ctxProxy
  , server
  )
import Freeze.Server.Reaper
  ( stopSessionReaper
  , startSessionReaper
  )

main :: IO ()
main = do
  getEnv "FREEZE_LOCATION" >>= setCurrentDirectory
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
