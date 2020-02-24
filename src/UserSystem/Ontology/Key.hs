module UserSystem.Ontology.Key where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID (toText)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData)
import System.Random (randomIO)

newtype Key a = Key { unKey :: Text }
  deriving stock (Generic, Typeable)
  deriving newtype (ToHttpApiData, FromHttpApiData, Eq, Show, Read, Ord, FromField, ToField, ToJSON, FromJSON)

freshKey :: MonadIO m => m (Key a) 
freshKey = liftIO ((Key . toText) <$> randomIO)
