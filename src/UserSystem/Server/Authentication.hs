module UserSystem.Server.Authentication where

import Data.Pool
import Network.Wai
import Database.PostgreSQL.Simple hiding ((:.))
import Servant
import UserSystem.Ontology
import UserSystem.Database
import Control.Monad.Reader
import Web.Cookie

authenticateRequest :: Pool Connection -> Request -> Handler User
authenticateRequest pool req = do
  cookie <- maybe
      (throwError err401 {errReasonPhrase = "You have no cookies"})
      return $ lookup "cookie" (requestHeaders req)
  token <- maybe
      (throwError err401 {errReasonPhrase = "You don't have a user cookie"})
      return $ lookup "user" $ parseCookiesText cookie
  maybe
    (throwError err401 {errReasonPhrase = "Could not validate token"})
    return =<< (runReaderT . unDatabaseT) (validateToken token) pool
