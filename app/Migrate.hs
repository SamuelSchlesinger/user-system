module Main where

import Control.Monad
  ( forM_
  , void )
import Control.Monad.Catch
  ( SomeException
  , catch )
import Control.Monad.Trans
  ( MonadIO(liftIO) )
import Data.List
  ( (\\) )
import Data.String
  ( fromString )
import Data.Time.Clock
  ( getCurrentTime ) 
import Database.PostgreSQL.Simple
  ( Query
  , execute_
  , execute )
import Database.PostgreSQL.Simple.SqlQQ
  ( sql )
import UserSystem.Database
  ( DatabaseT
  , runDatabaseT
  , testInfo
  , lookupExecutedMigrations
  , withConnection )
import UserSystem.Ontology
  ( executedMigrationFilePath )
import System.Environment
  ( getEnv )
import System.Posix.User
  ( getEffectiveUserName )

main :: IO ()
main = do
  username <- getEffectiveUserName
  runDatabaseT (testInfo username) do
    freezeLocation <- liftIO $ getEnv "USER_SYSTEM_LOCATION"
    migrations <- lines <$> liftIO (readFile (freezeLocation <> "/migrations/manifest"))
    executedMigrations <- catch (lookupExecutedMigrations migrations) (\(_ :: SomeException) -> return [])
    let migrationsToRun = migrations \\ map executedMigrationFilePath executedMigrations
    forM_ migrationsToRun runMigration

runMigration :: FilePath -> DatabaseT IO ()
runMigration filepath = withConnection \c -> do
  liftIO $ putStrLn $ "Running migration " <> filepath
  fileLocation <- 
    (<> "/migrations/" <> filepath) 
      <$> liftIO (getEnv "USER_SYSTEM_LOCATION")
  q :: Query <- fromString <$> liftIO (readFile fileLocation)
  void $ liftIO (execute_ c q)
  currentTime <- liftIO getCurrentTime
  void $ liftIO (execute c [sql|
      insert into executed_migrations (file_name, creation_date)
      values (?, ?);
    |] (filepath, currentTime))
