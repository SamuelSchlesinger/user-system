module UserSystem.Ontology where

import Data.Aeson
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
