module Main where

import Control.Monad
  ( void )
import Control.Monad.Trans
  ( MonadIO(liftIO) )
import Database.PostgreSQL.Simple
  ( execute )
import Database.PostgreSQL.Simple.SqlQQ
  ( sql )
import UserSystem.Database
  ( runDatabaseT 
  , testInfo 
  , withConnection )
import UserSystem.Ontology
  ( Role 
  , freshKey 
  , Key(unKey) )
import Data.Text
  ( pack )
import System.Posix.User
  ( getEffectiveUserName )
import System.Environment
  ( getArgs )

main :: IO ()
main = do
  [pack -> targetUsername, pack -> targetObject, read -> role :: Role] <- getArgs
  username <- getEffectiveUserName
  sessionID <- freshKey
  (unKey -> sessionToken) <- freshKey
  runDatabaseT (testInfo username) $ withConnection \c -> 
    void . liftIO $ execute c [sql|
      insert into sessions (id, owner, token, creation_date, expiration_date) 
      select ?, users.id, ?, now(), now() + interval '1 hour'
      from users where username = ?;
      insert into session_roles (session, role, object, creation_date)
      select ?, ?, objects.id, now()
      from objects where name = ?;
    |] (sessionID, sessionToken, targetUsername, sessionID, role, targetObject)

