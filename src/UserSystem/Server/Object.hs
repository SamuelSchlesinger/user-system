module UserSystem.Server.Object where

import Control.Monad.Except
import Data.Text.Encoding
import Data.Time.Clock
import Data.UUID
import UserSystem.Database
import UserSystem.Ontology
import UserSystem.Monad
import Servant
import System.Random
import UserSystem.API.Types

readObject :: MonadUserSystem m => User -> ReadObject -> m (Response ReadObject)
readObject User{userID} (ReadObject objectName) = do
  authedLookupObject userID objectName >>= \case
    Left ObjectDoesntExist -> throwError err404
    Left RoleViolation -> throwError err403
    Left UserDoesntExist -> throwError err404
    Right blob -> return $ ReadObjectResponse objectName blob

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
    Just UserDoesntExist -> throwError err404

giveUserRole :: MonadUserSystem m => User -> GiveUserRole -> m (Response GiveUserRole)
giveUserRole User{userID} (GiveUserRole{..}) = do
  updateUserRole userID giveUserRoleUsername giveUserRoleObject giveUserRoleRole >>= \case
    Nothing -> return GaveUserRole
    Just RoleViolation -> throwError err403
    Just ObjectDoesntExist -> throwError err404
    Just UserDoesntExist -> throwError err404
