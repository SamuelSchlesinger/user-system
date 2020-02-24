module Freeze.Server (freezeProxy, ctx, ctxProxy, server) where

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
import Freeze.API
  ( FreezeAPI
  , Ctx
  , AccountAPI
  , FreezerAPI
  , AuthenticatedAccountAPI )
import Freeze.Monad
  ( MonadFreeze )
import Freeze.Server.Account 
  ( signup
  , signin
  , newToken
  , changePassword
  , changeUsername )
import Freeze.Server.Freezer
  ( createFreezer
  , createProject
  , createBox
  , slotProbe
  , createMouse
  , createSample
  , createSampleType
  , getFreezers
  , createMouseProperty )
import Freeze.Server.Authentication
  ( authenticateRequest )
import Freeze.Ontology
  ( User )

freezeProxy :: Proxy FreezeAPI
freezeProxy = Proxy

ctxProxy :: Proxy Ctx
ctxProxy = Proxy

ctx :: Pool Connection -> Context Ctx
ctx pool = mkAuthHandler (authenticateRequest pool) :. EmptyContext

server :: MonadFreeze m => ServerT FreezeAPI m
server = account :<|> freezer :<|> serveDirectoryWebApp "static"

account :: MonadFreeze m => ServerT AccountAPI m
account = signup :<|> signin :<|> authenticatedAccount

authenticatedAccount :: MonadFreeze m => ServerT AuthenticatedAccountAPI m
authenticatedAccount user 
     = newToken user 
  :<|> changePassword user 
  :<|> changeUsername user 

freezer :: MonadFreeze m => User -> ServerT FreezerAPI m
freezer user
     = createFreezer user 
  :<|> createProject user
  :<|> createBox user
  :<|> slotProbe user
  :<|> createMouse user
  :<|> createSample user
  :<|> createSampleType user
  :<|> createMouseProperty user
  :<|> getFreezers user
