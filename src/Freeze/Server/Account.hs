module Freeze.Server.Account where

import Control.Monad.Except
  ( MonadIO(liftIO) )
import Data.Text
  ( Text )
import Data.ByteString
  ( ByteString )
import Data.Text.Encoding
  ( encodeUtf8 )
import Data.Time.Clock
  ( addUTCTime
  , getCurrentTime )
import qualified Crypto.BCrypt as BCrypt
import Crypto.BCrypt
  ( validatePassword )
import Data.UUID
  ( toText )
import Freeze.Database
  ( UpdateUsernameError(NewUsernameTaken)
  , lookupUsersByUsername
  , insertUsers
  , insertSessions
  , updateUserPasshash 
  , updateUsername )
import Freeze.Ontology
import Freeze.Monad
import Servant
import System.Random
import Web.Cookie
import Freeze.API.Types

signup :: MonadFreeze m => SignUp -> m (Response SignUp)
signup (SignUp username password) = do
  lookupUsersByUsername [username] >>= \case
    [] -> do
      userCreationDate <- liftIO getCurrentTime
      userID <- freshKey
      passhash <- hashPassword password
      insertUsers [User {..}]
      return SignedUp
    _ -> throwError err403 {errReasonPhrase = "Username already exists"}

hashPassword :: MonadFreeze m => Text -> m ByteString
hashPassword password = do
  x <- liftIO $ BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (encodeUtf8 password)
  maybe (throwError err500) return x

createSession :: MonadFreeze m => Key User -> m (WithCookieHeaders (Response SignIn))
createSession sessionOwner = do
  sessionCreationDate <- liftIO getCurrentTime
  let sessionExpirationDate = addUTCTime (60 * 60) sessionCreationDate
  sessionToken <- liftIO (toText <$> randomIO)
  sessionID <- freshKey
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

signin :: MonadFreeze m => SignIn -> m (WithCookieHeaders (Response SignIn))
signin (SignIn username password) =
  lookupUsersByUsername [username] >>= \case
    [User {userID, passhash}] -> do
      if (validatePassword passhash (encodeUtf8 password))
        then do
          createSession userID
        else throwError err403 {errReasonPhrase = "Wrong password"}
    _ -> throwError err403 {errReasonPhrase = "Wrong username"}

newToken :: MonadFreeze m => User -> m (WithCookieHeaders (Response SignIn))
newToken User{userID} = createSession userID 

changePassword :: MonadFreeze m => User -> ChangePassword -> m (Response ChangePassword)
changePassword User{userID} (ChangePassword newPassword) = do
  newPasshash <- hashPassword newPassword
  updateUserPasshash userID newPasshash >>= \case
    True -> return ChangedPassword
    False -> throwError err500

changeUsername :: MonadFreeze m => User -> ChangeUsername -> m (Response ChangeUsername)
changeUsername user (ChangeUsername newUsername) = do
  updateUsername user newUsername >>= \case
    Nothing -> return ChangedUsername
    Just NewUsernameTaken -> throwError err409
