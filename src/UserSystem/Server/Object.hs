module UserSystem.Server.Object where

import Control.Monad.Except
  ( MonadIO(liftIO) )
import Data.Text.Encoding
  ( encodeUtf8 )
import Data.Time.Clock
  ( getCurrentTime )
import UserSystem.Database
  ( ObjectAccessError(..)
  , authedLookupObject
  , insertObject
  , updateObjectContents
  , updateUserRole )
import UserSystem.Ontology
  ( User(..)
  , Object(..)
  , freshKey )
import UserSystem.Monad
  ( MonadUserSystem )
import Servant
  ( throwError
  , err404
  , err403
  , err409 )
import UserSystem.API.Types
  ( ReadObject(..)
  , Response(..)
  , CreateObject(..)
  , EditObject(..)
  , GiveUserRole(..) )

readObject :: MonadUserSystem m => User -> ReadObject -> m (Response ReadObject)
readObject User{userID} (ReadObject objectName) = do
  authedLookupObject userID objectName >>= \case
    Left ObjectDoesntExist -> throwError err404
    Left RoleViolation -> throwError err403
    Left UserDoesntExist -> throwError err404
    Left UpdateSelfError -> throwError err403
    Right blob -> return $ ReadObjectResponse objectName blob

createObject :: MonadUserSystem m => User -> CreateObject -> m (Response CreateObject)
createObject User{userID} (CreateObject objectName (encodeUtf8 -> objectContents)) = do
  objectID <- freshKey
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
    Just UserDoesntExist -> throwError err404
    Just UpdateSelfError -> throwError err403

giveUserRole :: MonadUserSystem m => User -> GiveUserRole -> m (Response GiveUserRole)
giveUserRole user (GiveUserRole{..}) = do
  updateUserRole user giveUserRoleUsername giveUserRoleObject giveUserRoleRole >>= \case
    Nothing -> return GaveUserRole
    Just RoleViolation -> throwError err403
    Just ObjectDoesntExist -> throwError err404
    Just UserDoesntExist -> throwError err404
    Just UpdateSelfError -> throwError err403
