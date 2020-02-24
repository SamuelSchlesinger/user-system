module Freeze.Ontology where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString hiding (pack, unpack)
import Data.Map (Map)
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

data SessionRole = SessionRole
  { sessionRoleSession :: Key Session
  , sessionRole :: Role
  , sessionRoleBox :: Key Box
  , sessionRoleCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow)

data UserRole = UserRole
  { userRoleUser :: Key User
  , userRole :: Key Role
  , userRoleBox :: Key Box
  , userRoleCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord, Typeable)
    deriving anyclass (FromRow, ToRow)

-- 4 shelves
type Shelf = Int

type Rack = Int

-- 5 down x 4 across
type Slot = (Int, Int)

data Box = Box
  { boxID :: Key Box
  , boxCode :: Text
  , boxProject :: Key Project
  , boxFreezer :: Key Freezer
  , boxShelf :: Shelf
  , boxRack :: Rack
  , boxSlotX :: Int
  , boxSlotY :: Int
  , boxCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data MousePropertyType = MousePropertyType
  { mousePropertyTypeName :: Key MousePropertyType
  , mousePropertyTypeDescription :: Text
  , mousePropertyTypeCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data MouseProperty = MouseProperty
  { mousePropertyType :: Key MousePropertyType
  , mousePropertyName :: Key MouseProperty
  , mousePropertyDescription :: Text
  , mousePropertyCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data Project = Project
  { projectID :: Key Project
  , projectName :: Text
  , projectDescription :: Text
  , projectCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data SampleType = SampleType
  { sampleTypeName :: Key SampleType
  , sampleTypeDescription :: Text
  , sampleTypeCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data Mouse = Mouse
  { mouseID :: Key Mouse
  , mouseTagID :: Text
  , mouseDateOfBirth :: UTCTime
  , mouseBox :: Key Box
  , mouseDateOfTissueCollection :: UTCTime
  , mouseCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data Sample = Sample
  { sampleID :: Key Sample
  , sampleCode :: Text
  , sampleMouse :: Key Mouse
  , sampleBox :: Key Box
  , sampleType :: Key SampleType
  , sampleNotes :: Text
  , sampleCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data Freezer = Freezer
  { freezerID :: Key Freezer
  , freezerCode :: Text
  , freezerName :: Text
  , freezerRackWidth :: Int
  , freezerRackHeight :: Int
  , freezerRacksPerShelf :: Int
  , freezerShelfNumber :: Shelf
  , freezerCreationDate :: UTCTime
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)

data FullFreezer = FullFreezer
  { fullFreezerFreezer :: Freezer
  , fullFreezerShelf :: Map Int FullShelf
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (ToJSON, FromJSON) 

data FullShelf = FullShelf
  { fullShelfRacks :: Map Int FullRack 
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (ToJSON, FromJSON) 

data FullRack = FullRack
  { fullRackBoxes :: Map (Int, Int) FullBox
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (ToJSON, FromJSON) 

data FullBox = FullBox
  { fullBoxBox :: Box
  , fullBoxSamples :: [Sample]
  } deriving stock (Generic, Eq, Show, Read, Ord)
    deriving anyclass (ToJSON, FromJSON)
