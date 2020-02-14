module UserSystem.Server (freezeProxy, ctx, ctxProxy, server, startSessionReaper, stopSessionReaper) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception hiding (Handler)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Pool
import Database.PostgreSQL.Simple hiding ((:.))
import Servant
import Servant.Server.Experimental.Auth
import UserSystem.API
import UserSystem.Database
import UserSystem.Monad
import UserSystem.Ontology
import UserSystem.Server.Account
import Web.Cookie
import qualified Network.Wai as Wai

freezeProxy :: Proxy UserSystemAPI
freezeProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Pool Connection -> Context Ctx
ctx pool = mkAuthHandler (authenticateRequest pool) :. EmptyContext

authenticateRequest :: Pool Connection -> Wai.Request -> Handler User
authenticateRequest pool req = do
  cookie <- maybe
      (throwError err401 {errReasonPhrase = "You have no cookies"})
      return $ lookup "cookie" (Wai.requestHeaders req)
  token <- maybe
      (throwError err401 {errReasonPhrase = "You don't have a user cookie"})
      return $ lookup "user" $ parseCookiesText cookie
  maybe
    (throwError err401 {errReasonPhrase = "Could not validate token"})
    return =<< (runReaderT . unDatabaseT) (validateToken token) pool

server :: MonadUserSystem m => ServerT UserSystemAPI m
server = account :<|> serveDirectoryWebApp "static"

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
