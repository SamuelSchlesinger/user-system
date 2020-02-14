module UserSystem.Server (freezeProxy, ctx, ctxProxy, server) where

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
