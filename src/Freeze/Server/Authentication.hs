module Freeze.Server.Authentication where

import Data.Pool
  ( Pool )
import Network.Wai 
  ( Request 
  , requestHeaders )
import Database.PostgreSQL.Simple 
  ( Connection )
import Servant 
  ( Handler
  , ServerError(errReasonPhrase)
  , throwError
  , err401 )
import Freeze.Ontology
  ( User )
import Freeze.Database
  ( DatabaseT(unDatabaseT)
  , validateToken )
import Control.Monad.Reader
  ( ReaderT(runReaderT) )
import Web.Cookie
  ( parseCookiesText )

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
