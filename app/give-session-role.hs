module Main where

import Control.Monad
import Control.Monad.Trans
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import UserSystem.Database
import UserSystem.Ontology
import Data.Text (pack)
import System.Posix.User
import System.Environment

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

