module UserSystem.Ontology.Object where

import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString hiding (pack, unpack)
import Data.Text (Text, unpack, pack)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (ToRow, FromRow)
import Database.PostgreSQL.Simple.FromField (FromField(fromField), typename)
import Database.PostgreSQL.Simple.ToField (ToField(toField), Action(Escape))
import GHC.Generics (Generic)
import Servant (FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece))
import Text.Read (readMaybe)
import UserSystem.Ontology.Key (Key(..))
import UserSystem.Ontology.Account (User, Session)

data Role = Read | Edit | Collaborator | Owner
  deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance FromHttpApiData Role where
  parseUrlPiece = maybeToEither . readMaybe . unpack where
    maybeToEither (Just a) = Right a
    maybeToEither Nothing = Left "Could not parse Role"

instance ToHttpApiData Role where
  toUrlPiece = pack . show

instance ToField Role where
  toField Read = Escape "Read"
  toField Edit = Escape "Edit"
  toField Collaborator = Escape "Collaborator"
  toField Owner = Escape "Owner"
  
instance FromField Role where
  fromField f mb = do
    n <- typename f
    if n == "role"
    then case mb >>= roleFromByteString of
            Nothing -> error "failed to marshall role"
            Just ps -> pure ps
    else error $ "expected type role, but got " <> show n
    where
      roleFromByteString "Read" = Just Read
      roleFromByteString "Edit" = Just Edit
      roleFromByteString "Collaborator" = Just Collaborator
      roleFromByteString "Owner" = Just Owner
      roleFromByteString _ = Nothing

data Object = Object
  { objectID :: Key Object
  , objectName :: Text
  , objectContents :: ByteString
  , objectCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow)

data SessionRole = SessionRole
  { sessionRoleSession :: Key Session
  , sessionRole :: Role
  , sessionRoleObject :: Key Object
  , sessionRoleCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow)

data UserRole = UserRole
  { userRoleUser :: Key User
  , userRole :: Key Role
  , userRoleObject :: Key Object
  , userRoleCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow)
