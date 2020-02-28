module Main where

import Control.Monad
  ( void )
import Control.Monad.Trans
  ( MonadIO(liftIO) )
import Database.PostgreSQL.Simple
  ( execute_ )
import Database.PostgreSQL.Simple.SqlQQ
  ( sql )
import UserSystem.Database
  ( MonadDatabase(withConnection) 
  , runDatabaseT 
  , testInfo )
import System.Posix.User
  ( getEffectiveUserName )

main :: IO ()
main = do
  username <- getEffectiveUserName
  runDatabaseT (testInfo username) $ withConnection \c -> 
    void . liftIO $ execute_ c [sql|
     drop table executed_migrations cascade;
     drop table users cascade;
     drop table sessions cascade;
    |]

