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
     drop table objects cascade;
     drop table session_roles cascade;
     drop table user_roles cascade;
     drop type role;
     drop table freezers cascade;
     drop table projects cascade;
     drop table boxes cascade;
     drop table mice cascade;
     drop table mouse_property_types cascade;
     drop table mouse_properties cascade;
     drop table sample_types cascade;
     drop table samples cascade;
     drop table join_mouse_properties;
    |]

