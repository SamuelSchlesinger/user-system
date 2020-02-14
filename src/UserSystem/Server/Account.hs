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
account = signup :<|> signin :<|> authenticatedAccount

authenticatedAccount :: MonadUserSystem m => ServerT AuthenticatedAccountAPI m
authenticatedAccount user 
     = newToken user 
  :<|> changePassword user 
  :<|> changeUsername user 
  :<|> createObject user 
  :<|> editObject user
  :<|> readObject user

readObject :: MonadUserSystem m => User -> ReadObject -> m (Response ReadObject)
readObject User{userID} (ReadObject objectName) = do
  authedLookupObject userID objectName >>= \case
    Left AuthError -> throwError err403
    Left ReadObjectDoesntExist -> throwError err404
    Left ReadRoleViolation -> throwError err403
    Right blob -> return $ ReadObjectResponse objectName blob

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

changePassword :: MonadUserSystem m => User -> ChangePassword -> m (Response ChangePassword)
changePassword User{userID} (ChangePassword newPassword) = do
  newPasshash <- hashPassword newPassword
  updateUserPasshash userID newPasshash >>= \case
    True -> return ChangedPassword
    False -> throwError err500

changeUsername :: MonadUserSystem m => User -> ChangeUsername -> m (Response ChangeUsername)
changeUsername User{userID} (ChangeUsername newUsername) = do
  updateUsername userID newUsername >>= \case
    Nothing -> return ChangedUsername
    Just NewUsernameTaken -> throwError err409

createObject :: MonadUserSystem m => User -> CreateObject -> m (Response CreateObject)
createObject User{userID} (CreateObject objectName (encodeUtf8 -> objectContents)) = do
  objectID <- liftIO ((Key . toText) <$> randomIO)
  objectCreationDate <- liftIO getCurrentTime
  insertObject userID Object{..} >>= \case
    True -> return CreatedObject
    False -> throwError err409

editObject :: MonadUserSystem m => User -> EditObject -> m (Response EditObject)
editObject User{userID} (EditObject objectName (encodeUtf8 -> objectContents)) = 
  updateObjectContents userID objectName objectContents >>= \case
    Nothing -> return EditedObject
    Just RoleViolation -> throwError err403
    Just ObjectDoesntExist -> throwError err404
