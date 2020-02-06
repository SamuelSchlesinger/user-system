module UserSystem.Server (freezeProxy, ctx, ctxProxy, server) where

import Control.Monad.Except
import Database.PostgreSQL.Simple hiding ((:.))
import UserSystem.API
import UserSystem.Database
import UserSystem.Monad
import UserSystem.Ontology
import UserSystem.Server.Account
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie
import qualified Network.Wai as Wai

freezeProxy :: Proxy UserSystemAPI
freezeProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Connection -> Context Ctx
ctx conn = mkAuthHandler validate :. EmptyContext
  where
    validate :: Wai.Request -> Handler User
    validate req = do
      cookie <- maybe
          (throwError err401 {errReasonPhrase = "You have no cookies"})
          return $ lookup "cookie" (Wai.requestHeaders req)
      token <- maybe
          (throwError err401 {errReasonPhrase = "You don't have a user cookie"})
          return $ lookup "user" $ parseCookiesText cookie
      maybe
        (throwError err401 {errReasonPhrase = "Could not validate token"})
        return =<< runSharedDatabaseT conn (validateToken token)

server :: MonadUserSystem m => ServerT UserSystemAPI m
server = account :<|> serveDirectoryWebApp "static"
