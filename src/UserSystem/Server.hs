module UserSystem.Server (freezeProxy, ctx, ctxProxy, server) where

import Data.Pool 
  ( Pool )
import Database.PostgreSQL.Simple 
  ( Connection )
import Servant 
  ( Proxy(Proxy)
  , Context
  , ServerT
  , (:<|>)((:<|>))
  , serveDirectoryWebApp
  , Context((:.), EmptyContext) )
import Servant.Server.Experimental.Auth 
  ( mkAuthHandler )
import UserSystem.API
  ( UserSystemAPI
  , Ctx
  , AccountAPI
  , ObjectAPI
  , AuthenticatedAccountAPI )
import UserSystem.Monad 
  ( MonadUserSystem)
import UserSystem.Server.Account 
  ( signup
  , signin
  , newToken
  , changePassword
  , changeUsername )
import UserSystem.Server.Object 
  ( createObject
  , editObject
  , readObject
  , giveUserRole )
import UserSystem.Server.Authentication
  ( authenticateRequest )

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
