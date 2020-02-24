module Freeze.Seed where

import Freeze.Ontology
import Freeze.Database
import Control.Monad.Trans.Control
import Data.Time
import Control.Exception.Lifted

beginning :: UTCTime
beginning = UTCTime (toEnum 0) 0

users :: [User]
users = [ User (Key "1") "samuel" "giuseppe" beginning
        , User (Key "2") "samantha" "grace" beginning
        ]

freezers :: [Freezer]
freezers = [ Freezer {
               freezerID = Key "1"
             , freezerCode = "code1"
             , freezerName = "sam's freezer" 
             , freezerRackWidth = 4
             , freezerRackHeight = 5
             , freezerRacksPerShelf = 5
             , freezerShelfNumber = 4
             , freezerCreationDate = beginning
             }
           , Freezer {
                freezerID = Key "2"
              , freezerCode = "code2"
              , freezerName = "monica's freezer"
              , freezerRackWidth = 4
              , freezerRackHeight = 5
              , freezerRacksPerShelf = 5
              , freezerShelfNumber = 4
              , freezerCreationDate = beginning
             }
           ]

projects :: [Project]
projects = [ Project {
               projectID = Key "1"
             , projectName = "Mucolipidosis Type 4"
             , projectDescription = "Mouse studies for Mucolipidosis Type 4"
             , projectCreationDate = beginning
             }
           ]

sampleTypes :: [SampleType]
sampleTypes = [ SampleType (Key name) ("Description of " <> name) beginning | name <- ["Liver", "Kidney", "Brain Stem"] ]

mousePropertyTypes :: [MousePropertyType]
mousePropertyTypes = [ MousePropertyType (Key name) ("Description of " <> name) beginning | name <- ["Strain", "Genotype"] ]

seed :: MonadDatabase m => m ()
seed = do
  insertUsers users
  insertProjects projects
  insertFreezers freezers
  insertSampleTypes sampleTypes
  insertMousePropertyTypes mousePropertyTypes

cleanup :: MonadDatabase m => m ()
cleanup = do
  deleteUsers (map userID users)
  deleteProjects (map projectID projects)
  deleteFreezers (map freezerID freezers)
  deleteSampleTypes (map sampleTypeName sampleTypes)
  deleteMousePropertyTypes (map mousePropertyTypeName mousePropertyTypes)
  return ()

withSeed :: (MonadBaseControl IO m, MonadDatabase m) => m a -> m a
withSeed a = bracket seed (const cleanup) (const a)
