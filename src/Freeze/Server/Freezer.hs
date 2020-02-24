module Freeze.Server.Freezer where

import Servant
import Freeze.Monad
import qualified Freeze.API.Types as API
import Freeze.Database
import Control.Monad.IO.Class
import Data.Time.Clock
import System.Random
import Data.UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Freeze.Ontology

-- | A 'RequestHandler request' is parameterized by a 'User', a 'request',
-- and returns a 'Response request'.
type RequestHandler request = forall m. MonadFreeze m => User -> request -> m (API.Response request)
 
getFreezers :: MonadFreeze m => User -> API.GetFreezers -> m (API.Response API.GetFreezers)
getFreezers _ (API.GetFreezers freezers) = do
  case freezers of
    Nothing -> API.GotFreezers <$> lookupAllFullFreezers
    Just xs -> API.GotFreezers <$> lookupFullFreezers xs

createSample :: RequestHandler API.CreateSample
createSample user (API.CreateSample sampleMouse sampleBox sampleType sampleNotes) = do
  sampleID <- liftIO ((Key . toText) <$> randomIO)
  sampleCode <- liftIO (toText <$> randomIO)
  sampleCreationDate <- liftIO getCurrentTime
  let sample = Sample{..}
  lookupBoxes [sampleBox] >>= \case
    [Box{..}] -> slotProbe user (API.SlotProbe {
        slotProbeFreezer = boxFreezer 
      , slotProbeShelf = boxShelf
      , slotProbeSlotX = boxSlotX
      , slotProbeSlotY = boxSlotY
      , slotProbeRack  = boxRack
      }) >>= \case
      API.SlotOccupied _ -> API.CreatedSample sample <$ insertSamples [sample]
      API.SlotUnoccupied -> throwError err400 {errReasonPhrase="Cannot insert sample into empty slot"}
      API.SlotInvalid    -> throwError err400 {errReasonPhrase="Cannot insert sample into invalid slot"}
    [] -> throwError err400 {errReasonPhrase="Cannot insert sample into box that doesn't exist"}
    _ -> throwError err500 {errReasonPhrase="Multiple boxes with same key invalidates foreign key constraints"}

createMouseProperty :: RequestHandler API.CreateMouseProperty
createMouseProperty _ (API.CreateMouseProperty mousePropertyType mousePropertyName mousePropertyDescription) = do
  mousePropertyCreationDate <- liftIO getCurrentTime
  let mouseProperty = MouseProperty{..}
  lookupMouseProperties mousePropertyType [mousePropertyName] >>= \case
    [_] -> throwError err400 {errReasonPhrase="A mouse property already exists with this name"}
    (_ : _) -> throwError err500 {errReasonPhrase="Multiple mouse properties with the same key invalidates foreign key constraints"} 
    []  -> API.CreatedMouseProperty mouseProperty <$ insertMouseProperties [mouseProperty]

createSampleType :: RequestHandler API.CreateSampleType
createSampleType _ (API.CreateSampleType sampleTypeName sampleTypeDescription) = do
  sampleTypeCreationDate <- liftIO getCurrentTime
  let sampleType = SampleType{..}
  lookupSampleTypes [sampleTypeName] >>= \case
    [_] -> throwError err400 {errReasonPhrase="A sample type already exists with this name"}
    (_ : _) -> throwError err500 {errReasonPhrase="Multiple sample types with the same key invalidates foreign key constraints"} 
    []  -> API.CreatedSampleType sampleType <$ insertSampleTypes [sampleType]

createMouse :: RequestHandler API.CreateMouse
createMouse user cm@(API.CreateMouse mouseTagID mouseDateOfBirth mouseBox mouseDateOfTissueCollection) = do
  mouseID <- liftIO ((Key . toText) <$> randomIO)
  mouseCreationDate <- liftIO getCurrentTime
  let mouse = Mouse{..}
  lookupMice [mouseID] >>= \case
    [_] -> createMouse user cm -- random probing for an empty slot
    (_ : _) -> throwError err500 {errReasonPhrase="Multiple sample types with the same key invalidates foreign key constraints"} 
    []  -> API.CreatedMouse mouse <$ insertMice [mouse]

createFreezer :: RequestHandler API.CreateFreezer
createFreezer _ (API.CreateFreezer freezerName freezerRackWidth freezerRackHeight freezerRacksPerShelf freezerShelfNumber) = lookupFreezersByNames [freezerName] >>= \case
    [_] -> throwError err400 {errReasonPhrase = "Freezer by that name already exists"}
    (_ : _) -> throwError err500 {errReasonPhrase = "Two freezers by the same name existing violates uniqueness constraint"}
    [] -> go where
            go = do
              freezerCode <- liftIO (toText <$> randomIO)
              freezerCreationDate <- liftIO getCurrentTime
              freezerID <- liftIO ((Key . toText) <$> randomIO)
              let freezer' = Freezer{..}
              lookupFreezers [freezerID] >>= \case
                [_] -> go -- random probing for an empty slot
                (_ : _) -> throwError err500 {errReasonPhrase="Multiple freezers with the same key invalidates foreign key constraints"} 
                []  -> API.CreatedFreezer freezer' <$ insertFreezers [freezer']

createProject :: RequestHandler API.CreateProject
createProject _ (API.CreateProject projectName projectDescription) = do
  lookupProjectsByNames [projectName] >>= \case
    [] -> do
      projectID <- liftIO ((Key . toText) <$> randomIO)
      projectCreationDate <- liftIO getCurrentTime
      let project = Project{..}
      API.CreatedProject project <$ insertProjects [project]
    _ -> throwError err400 {errReasonPhrase = "Project by that name already exists"}

createBox :: RequestHandler API.CreateBox
createBox user (API.CreateBox boxProject boxFreezer boxShelf boxRack boxSlotX boxSlotY) = do
  slotProbe user (API.SlotProbe boxFreezer boxShelf boxRack boxSlotX boxSlotY) >>= \case
    API.SlotUnoccupied -> do
      boxCode <- liftIO (toText <$> randomIO)
      boxID <- liftIO ((Key . toText) <$> randomIO)
      boxCreationDate <- liftIO getCurrentTime
      let box = Box{..}
      API.CreatedBox box <$ insertBoxes [box]
    API.SlotOccupied _ -> throwError err400 {errReasonPhrase = "You tried to place a box into an occupied slot"}
    API.SlotInvalid -> throwError err400 {errReasonPhrase = "You tried to place a box into an invalid slot"}

slotProbe :: RequestHandler API.SlotProbe
slotProbe _ (API.SlotProbe freezer' shelf rack slotX slotY) = withConnection \c -> do
  Freezer{..} <- lookupFreezers [freezer'] >>= \case
    [] -> throwError err400 {errReasonPhrase = "You tried to probe a slot of a freezer that doesn't exist"}
    (a : []) -> return a
    (_ : _) -> throwError err500 {errReasonPhrase = "There is more than one freezer corresponding to the same key, violating uniqueness constraints"}
  results :: Maybe Box <- fmap headMay . liftIO $ query c [sql|
    select * from boxes where freezer = ?, shelf = ?, rack = ?, slot_x = ?, slot_y = ?;
  |] (freezer', shelf, rack, slotX, slotY)
  maybe (if slotX >= 0 && slotX < freezerRackWidth && slotY >= 0 && slotY < freezerRackHeight
           then return API.SlotUnoccupied
           else return API.SlotInvalid) 
        (pure . API.SlotOccupied) results
  where
    headMay (a : _) = Just a
    headMay [] = Nothing 
