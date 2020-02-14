module Main where

import Control.Monad
import Control.Monad.Trans
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import UserSystem.Database
import System.Posix.User

main :: IO ()
main = do
  username <- getEffectiveUserName
  runDatabaseT (testInfo username) $ withConnection \c -> 
    void . liftIO $ execute_ c [sql|
     drop table executed_migrations cascade;
     drop table users cascade;
     drop table sessions;
    |]

