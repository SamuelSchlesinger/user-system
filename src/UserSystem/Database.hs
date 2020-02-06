{-# LANGUAGE UndecidableInstances #-}
module UserSystem.Database where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import UserSystem.Ontology

testInfo :: String -> ConnectInfo
testInfo username = ConnectInfo { 
      connectHost = "localhost"
    , connectPort = 5432
    , connectUser = username
    , connectPassword = ""
    , connectDatabase = "user-system"
    }

runTestDatabaseT :: (MonadIO m, MonadMask m) => String -> DatabaseT m a -> m a
runTestDatabaseT username (DatabaseT (ReaderT r)) 
  = bracket (liftIO (connect (testInfo username))) (liftIO . close) r

runDatabaseT :: (MonadIO m, MonadMask m) => ConnectInfo -> DatabaseT m a -> m a
runDatabaseT i (DatabaseT (ReaderT r)) = bracket (liftIO $ connect i) (liftIO . close) r

runSharedDatabaseT :: (MonadIO m, MonadMask m) => Connection -> DatabaseT m a -> m a
runSharedDatabaseT i (DatabaseT (ReaderT r)) = r i

newtype DatabaseT m a = DatabaseT { unDatabaseT :: ReaderT Connection m a }
  deriving newtype (MonadReader Connection, Monad, Functor, Applicative, MonadIO, MonadTrans, MonadMask, MonadThrow, MonadCatch, Alternative, MonadPlus)

instance (MonadError e m, MonadCatch m, Exception e) => MonadError e (DatabaseT m) where
  throwError = lift . throwError
  catchError a f = catch a f

class MonadIO m => MonadDatabase m where
  withConnection :: (Connection -> m a) -> m a

instance MonadIO m => MonadDatabase (DatabaseT m) where
  withConnection = (ask >>=)

insertUsers :: MonadDatabase m => [User] -> m ()
insertUsers users = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into users (id, username, passhash, creation_date) 
    values (?, ?, ?, ?);
   |] users

insertSessions :: MonadDatabase m => [Session] -> m ()
insertSessions sessions = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into sessions (id, owner, creation_date, token)
    values (?, ?, ?, ?);
    |] ((\Session{..} -> 
       (sessionID, sessionOwner, sessionCreationDate, sessionToken)) 
       <$> sessions)

insertExecutedMigrations :: MonadDatabase m => [ExecutedMigration] -> m ()
insertExecutedMigrations executedMigrations = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into executed_migrations (id, file_name)
    values (?, ?);
  |] executedMigrations

validateToken :: MonadDatabase m => Text -> m (Maybe User)
validateToken token = withConnection \c -> do
  (liftIO $ query c [sql|
    select users.id, users.username, users.passhash, users.creation_date
    from users
    inner join sessions on owner=users.id
                        and token in ?
                      where sessions.creation_date >= now() - interval '1 hour';
    |] (Only (In [token]))) >>= \case
      [] -> do
        return Nothing
      [s] -> do
        return (Just s)
      _ -> error "database has two sessions with the same token" 
      
lookupUsers :: MonadDatabase m => [Key User] -> m [User]
lookupUsers users = withConnection \c -> do
  liftIO $ query c [sql|
     select * from users where id in ?;
    |] (Only (In users))

lookupUsersByUsername :: MonadDatabase m => [Text] -> m [User]
lookupUsersByUsername usernames = withConnection \c -> do
  liftIO $ query c [sql|
     select * from users where username in ?;
    |] (Only (In usernames))

lookupExecutedMigrations :: MonadDatabase m => [FilePath] -> m [ExecutedMigration]
lookupExecutedMigrations names = withConnection \c -> do
  liftIO $ query c [sql|
    select * from executed_migrations where file_name in ?;
  |] (Only (In names))

deleteUsers :: MonadDatabase m => [Key User] -> m Int
deleteUsers users = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
     delete from users where id in ?;
    |] (Only (In users))

withTestUsers :: (MonadDatabase m, MonadMask m) => [User] -> ([Key User] -> m a) -> m a
withTestUsers users dbGo = bracket (insertUsers users) (const . void $ deleteUsers userKeys) (const $ dbGo userKeys) where
  userKeys = map userID users
