{-# LANGUAGE UndecidableInstances #-}
module UserSystem.Database where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.ByteString (ByteString)
import Data.Pool
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
  = bracket (createDatabasePool (testInfo username)) (liftIO . destroyAllResources) r

createDatabasePool :: MonadIO m => ConnectInfo -> m (Pool Connection)
createDatabasePool i = liftIO $ createPool (connect i) close 4 5 10

runDatabaseT :: (MonadIO m, MonadMask m) => ConnectInfo -> DatabaseT m a -> m a
runDatabaseT i (DatabaseT (ReaderT r)) = bracket (createDatabasePool i) (liftIO . destroyAllResources) r

newtype DatabaseT m a = DatabaseT { unDatabaseT :: ReaderT (Pool Connection) m a }
  deriving newtype (MonadReader (Pool Connection), Monad, Functor, Applicative, MonadIO, MonadTrans, MonadMask, MonadThrow, MonadCatch, Alternative, MonadPlus)

instance (MonadError e m, MonadCatch m, Exception e) => MonadError e (DatabaseT m) where
  throwError = lift . throwError
  catchError a f = catch a f

class MonadIO m => MonadDatabase m where
  withConnection :: (Connection -> m a) -> m a

instance (MonadMask m, MonadIO m) => MonadDatabase (DatabaseT m) where
  withConnection a = do
    p <- ask
    mask $ \restore -> do
      (resource, localPool) <- liftIO $ takeResource p
      ret <- restore (a resource) `catch` throwM @_ @SomeException
      liftIO $ putResource localPool resource
      return ret
    

insertUsers :: MonadDatabase m => [User] -> m ()
insertUsers users = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into users (id, username, passhash, creation_date) 
    values (?, ?, ?, ?);
   |] users

updateUserPasshash :: MonadDatabase m => Key User -> ByteString -> m Bool
updateUserPasshash user passhash = withConnection \c -> do
  lookupUsers [user] >>= \case
    [_] -> do
      void . liftIO $ execute c [sql|
        update users
        set passhash = ?
        where users.id = ?; 
      |] (passhash, user)
      return True
    [] -> return False
    (_ : _) -> error "We have more than one user associated with this key"

updateUsername :: MonadDatabase m => Key User -> Text -> m Bool
updateUsername user username = withConnection \c -> do
  lookupUsers [user] >>= \case
    [_] -> do
      void . liftIO $ execute c [sql|
        update users
        set username = ?
        where users.id = ?;
      |] (username, user)
      return True
    [] -> return False
    (_ : _) -> error "We have more than one user associated with this key"
    

insertSessions :: MonadDatabase m => [Session] -> m ()
insertSessions sessions = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into sessions (id, owner, creation_date, token)
    values (?, ?, ?, ?);
    |] sessions

insertExecutedMigrations :: MonadDatabase m => [ExecutedMigration] -> m ()
insertExecutedMigrations executedMigrations = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into executed_migrations (id, file_name)
    values (?, ?);
  |] executedMigrations

reapSessions :: MonadDatabase m => m ()
reapSessions = withConnection \c -> do
  void . liftIO $ execute_ c [sql|
    delete from sessions where sessions.creation_date < now() - interval '1 hour';
    |]

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
