module UserSystem.Ontology.ExecutedMigration where

import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Dynamic (Typeable)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Data.Aeson (ToJSON, FromJSON)

data ExecutedMigration = ExecutedMigration
  { executedMigrationFilePath :: FilePath
  , executedMigrationTimestamp :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

