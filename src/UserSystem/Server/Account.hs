module UserSystem.Server.Account (account) where

import Control.Monad.Except
import Data.Text
import Data.ByteString
import Data.Text.Encoding
import Data.Time.Clock
import qualified Crypto.BCrypt as BCrypt
import Crypto.BCrypt (validatePassword)
import Data.UUID
import UserSystem.API
import UserSystem.Database
import UserSystem.Ontology
import UserSystem.Monad
import Servant
import System.Random
import Web.Cookie
import UserSystem.API.Types

account :: MonadUserSystem m => ServerT AccountAPI m
account = signup :<|> signin :<|> newToken

signup :: MonadUserSystem m => SignUp -> m (Response SignUp)
signup (SignUp username password) = do
  lookupUsersByUsername [username] >>= \case
    [] -> do
      userCreationDate <- liftIO getCurrentTime
      userID <- liftIO ((Key . toText) <$> randomIO)
      passhash <- hashPassword password
      insertUsers [User {..}]
      return SignedUp
    _ -> throwError err403 {errReasonPhrase = "Username already exists"}

hashPassword :: MonadUserSystem m => Text -> m ByteString
hashPassword password = do
  x <- liftIO $ BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (encodeUtf8 password)
  maybe (throwError err500) return x

createSession :: MonadUserSystem m => Key User -> m (WithCookieHeaders (Response SignIn))
createSession sessionOwner = do
  sessionCreationDate <- liftIO getCurrentTime
  sessionToken <- liftIO (toText <$> randomIO)
  sessionID <- liftIO ((Key . toText) <$> randomIO)
  let sess = Session {..}
  insertSessions [sess]
  return $
    addHeader
      (def
         { setCookieName = "user"
         , setCookieSameSite = Just sameSiteStrict
         , setCookieMaxAge = Nothing
         , setCookieHttpOnly = True
         , setCookiePath = Just "/"
         , setCookieValue = encodeUtf8 sessionToken
         }) $
    addHeader "http://localhost:8080" $
    addHeader "Accept" $
    addHeader True $ SignedIn

signin :: MonadUserSystem m => SignIn -> m (WithCookieHeaders (Response SignIn))
signin (SignIn username password) =
  lookupUsersByUsername [username] >>= \case
    [User {userID, passhash}] -> do
      if (validatePassword passhash (encodeUtf8 password))
        then do
          createSession userID
        else throwError err403 {errReasonPhrase = "Wrong password"}
    _ -> throwError err403 {errReasonPhrase = "Wrong username"}

newToken :: MonadUserSystem m => User -> m (WithCookieHeaders (Response SignIn))
newToken User{userID} = createSession userID 
