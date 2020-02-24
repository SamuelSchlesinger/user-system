module Freeze.API.Types where

import Data.Aeson 
  ( FromJSON, ToJSON )
import Data.Time.Clock
  ( UTCTime )
import Data.Text
  ( Text )
import GHC.Generics 
  ( Generic )
import Web.FormUrlEncoded
  ( ToForm, FromForm )
import Servant
  ( Headers, Header )
import Web.Cookie
  ( SetCookie )
import Freeze.Ontology 
  ( Role
  , Key(..)
  , FullFreezer(..) 
  , Rack
  , Freezer(..)
  , Project(..) 
  , Shelf 
  , Box(..) 
  , Mouse(..) 
  , SampleType(..)
  , Sample(..) 
  , MousePropertyType(..) 
  , MouseProperty(..) )
import qualified Text.Blaze.Html5 as H
import Text.Blaze (ToMarkup(toMarkup))

type WithCookieHeaders res
   = Headers '[ Header "Set-Cookie" SetCookie
              , Header "Access-Control-Allow-Origin" String
              , Header "Access-Control-Allow-Headers" String
              , Header "Access-Control-Allow-Credentials" Bool ] res

data family Response a

type family Request b where
  Request (Response a) = a

data SignUp = SignUp
  { signUpUsername :: Text
  , signUpPassword :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response SignUp = SignedUp
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SignIn = SignIn
  { signInUsername :: Text
  , signInPassword :: Text }
  deriving  (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response SignIn = SignedIn
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChangePassword = ChangePassword
  { changePasswordNewPassword :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response ChangePassword = ChangedPassword
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChangeUsername = ChangeUsername
  { changeUsernameNewUsername :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response ChangeUsername = ChangedUsername
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateObject = CreateObject
  { createObjectName :: Text
  , createObjectContents :: Text } 
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateObject = CreatedObject
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data EditObject = EditObject
  { editObjectName :: Text
  , editObjectContents :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response EditObject = EditedObject
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ReadObject = ReadObject
  { readObjectName :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response ReadObject = ReadObjectResponse
  { readObjectResponseName :: Text
  , readObjectResponseContent :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GiveUserRole = GiveUserRole
  { giveUserRoleUsername :: Text
  , giveUserRoleObject :: Text
  , giveUserRoleRole :: Role }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, FromForm, ToForm)

data instance Response GiveUserRole = GaveUserRole
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GiveSessionRole = GiveSessionRole
  { giveSessionRoleUsername :: Text
  , giveSessionRoleObject :: Text
  , giveSessionRoleRole :: Role }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, FromForm, ToForm)

data instance Response GiveSessionRole = GaveSessionRole
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateFreezer = CreateFreezer
  { createFreezerName :: Text
  , createFreezerRackWidth :: Int
  , createFreezerRackHeight :: Int
  , createFreezerRacksPerShelf :: Rack
  , createFreezerShelfNumber :: Int
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateFreezer = CreatedFreezer Freezer
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateProject = CreateProject
  { createProjectName :: Text
  , createProjectDescription :: Text
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateProject = CreatedProject Project
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToMarkup (Response CreateProject) where
  toMarkup (CreatedProject Project{..}) = do
    H.head $ do
      H.title "Freeze"
    H.body $ do
      H.ul $ do
        H.li (H.text "Project ID: " <> H.text (unKey projectID))
        H.li (H.text "Project Name: " <> H.text projectName)
        H.li (H.text "Project Description: " <> H.text projectDescription)

data CreateBox = CreateBox
  { createBoxProject :: Key Project
  , createBoxFreezer :: Key Freezer
  , createBoxShelf :: Shelf
  , createBoxRack :: Rack
  , createBoxSlotX :: Int
  , createBoxSlotY :: Int
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateBox = CreatedBox Box
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SlotProbe = SlotProbe
  { slotProbeFreezer :: Key Freezer
  , slotProbeShelf :: Shelf
  , slotProbeRack :: Rack
  , slotProbeSlotX :: Int
  , slotProbeSlotY :: Int
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response SlotProbe = SlotOccupied Box | SlotUnoccupied | SlotInvalid
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateSample = CreateSample
  { createSampleMouse :: Key Mouse
  , createSampleBox :: Key Box
  , createSampleType :: Key SampleType
  , createSampleNotes :: Text
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateSample = CreatedSample Sample
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateSampleType = CreateSampleType
  { createSampleTypeName :: Key SampleType
  , createSampleTypeDescription :: Text 
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateSampleType = CreatedSampleType SampleType
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateMouse = CreateMouse
  { createMouseTagID :: Text
  , createMouseDateOfBirth :: UTCTime
  , createMouseBox :: Key Box
  , createMouseDateOfTissueCollection :: UTCTime
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateMouse = CreatedMouse Mouse
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateMousePropertyType = CreateMousePropertyType
  { createMousePropertyTypeName :: Text
  , createMousePropertyTypeDescription :: Text
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateMousePropertyType = CreatedMousePropertyType MousePropertyType
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
  
data CreateMouseProperty = CreateMouseProperty
  { createMousePropertyType :: Key MousePropertyType
  , createMousePropertyName :: Key MouseProperty
  , createMousePropertyDescription :: Text
  } deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response CreateMouseProperty = CreatedMouseProperty MouseProperty
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype GetFreezers = GetFreezers
 { getFreezers :: Maybe [Key Freezer] 
 } deriving stock (Eq, Ord, Show, Read, Generic)
   deriving anyclass (FromJSON, ToJSON)

data instance Response GetFreezers = GotFreezers [FullFreezer]
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
