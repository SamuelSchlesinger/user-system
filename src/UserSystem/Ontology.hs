module UserSystem.Ontology where

import Data.Aeson (ToJSON, FromJSON)
import Data.Time
import Data.Text
import Data.ByteString
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Servant

newtype Key a = Key { unKey :: Text }
  deriving stock (Generic)
  deriving newtype (ToHttpApiData, FromHttpApiData, Eq, Show, Read, Ord, FromField, ToField, ToJSON, FromJSON)

data Session = Session
  { sessionID :: Key Session
  , sessionOwner :: Key User
  , sessionCreationDate :: UTCTime
  , sessionToken :: Text
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)

data User = User
  { userID :: Key User
  , username :: Text
  , passhash :: ByteString
  , userCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)

data ExecutedMigration = ExecutedMigration
  { executedMigrationFilePath :: FilePath
  , executedMigrationTimestamp :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data Role = Read | Edit | Collaborator | Owner
  deriving stock (Generic, Eq, Show, Read, Ord)

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
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)

data SessionRole = SessionRole
  { sessionRoleSession :: Key Session
  , sessionRole :: Role
  , sessionRoleObject :: Key Object
  , sessionRoleCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)

data UserRole = UserRole
  { userRoleUser :: Key User
  , userRole :: Key Role
  , userRoleObject :: Key Object
  , userRoleCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow)
