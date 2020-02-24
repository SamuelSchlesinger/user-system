module UserSystem.Ontology.Account where

import Data.ByteString hiding (pack, unpack)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (ToRow, FromRow)
import GHC.Generics (Generic)
import UserSystem.Ontology.Key

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
