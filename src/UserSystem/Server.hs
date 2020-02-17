module UserSystem.Server (freezeProxy, ctx, ctxProxy, server) where

import Data.Pool
import Database.PostgreSQL.Simple hiding ((:.))
import Servant
import Servant.Server.Experimental.Auth
import UserSystem.API
import UserSystem.Monad
import UserSystem.Server.Account
import UserSystem.Server.Object
import UserSystem.Server.Authentication

freezeProxy :: Proxy UserSystemAPI
freezeProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Pool Connection -> Context Ctx
ctx pool = mkAuthHandler (authenticateRequest pool) :. EmptyContext

server :: MonadUserSystem m => ServerT UserSystemAPI m
server = account :<|> object :<|> serveDirectoryWebApp "static"

account :: MonadUserSystem m => ServerT AccountAPI m
account = signup :<|> signin :<|> authenticatedAccount

authenticatedAccount :: MonadUserSystem m => ServerT AuthenticatedAccountAPI m
authenticatedAccount user 
     = newToken user 
  :<|> changePassword user 
  :<|> changeUsername user 

object :: MonadUserSystem m => ServerT ObjectAPI m
object user
     = createObject user 
  :<|> editObject user
  :<|> readObject user
  :<|> giveUserRole user
