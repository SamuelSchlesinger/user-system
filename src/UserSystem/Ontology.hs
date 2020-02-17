module UserSystem.Ontology where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString hiding (pack, unpack)
import Data.Text (Text, unpack, pack)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.UUID (toText)
import Database.PostgreSQL.Simple (ToRow, FromRow)
import Database.PostgreSQL.Simple.FromField (FromField(fromField), typename)
import Database.PostgreSQL.Simple.ToField (ToField(toField), Action(Escape))
import GHC.Generics (Generic)
import Servant (FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece))
import System.Random (randomIO)
import Text.Read (readMaybe)

newtype Key a = Key { unKey :: Text }
  deriving stock (Generic, Typeable)
  deriving newtype (ToHttpApiData, FromHttpApiData, Eq, Show, Read, Ord, FromField, ToField, ToJSON, FromJSON)

freshKey :: MonadIO m => m (Key a) 
freshKey = liftIO ((Key . toText) <$> randomIO)

data Session = Session
  { sessionID :: Key Session
  , sessionOwner :: Key User
  , sessionCreationDate :: UTCTime
  , sessionToken :: Text
  , sessionExpirationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow)

data User = User
  { userID :: Key User
  , username :: Text
  , passhash :: ByteString
  , userCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow)

data ExecutedMigration = ExecutedMigration
  { executedMigrationFilePath :: FilePath
  , executedMigrationTimestamp :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

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
