{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module UserSystem.Database where

import Control.Applicative 
  ( Alternative )
import Control.Monad
  ( void )
import Control.Monad.Catch
  ( MonadMask
  , MonadThrow
  , MonadCatch
  , Exception
  , SomeException
  , bracket
  , catch
  , throwM
  , mask )
import Control.Monad.Except
  ( MonadError(catchError, throwError)
  , MonadIO(liftIO)
  , lift
  , MonadTrans
  , MonadPlus )
import Control.Monad.Reader.Class 
  ( MonadReader(ask) )
import Control.Monad.Trans.Reader
  ( ReaderT(..) )
import Data.ByteString
  ( ByteString )
import Data.Maybe
  ( listToMaybe )
import Data.Pool
  ( Pool
  , destroyAllResources
  , createPool
  , putResource
  , takeResource
  , withResource )
import Data.Text
  ( Text )
import Data.Text.Encoding
  ( decodeUtf8 )
import Database.PostgreSQL.Simple
  ( ConnectInfo(..)
  , Connection
  , connect
  , close
  , withTransaction
  , executeMany
  , query
  , execute
  , execute_ 
  , Only(..) 
  , In(..) )
import Database.PostgreSQL.Simple.SqlQQ
  ( sql )
import UserSystem.Ontology
  ( User(..)
  , Key(..)
  , Session(..)
  , Object(..)
  , Role(..)
  , ExecutedMigration(..) )
import Data.Typeable
  ( Typeable, typeRep, Proxy(..) )

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
  inTransaction :: IO a -> m a

instance (MonadMask m, MonadIO m) => MonadDatabase (DatabaseT m) where
  withConnection a = do
    p <- ask
    mask $ \restore -> do
      (resource, localPool) <- liftIO $ takeResource p
      ret <- restore (a resource) `catch` throwM @_ @SomeException
      liftIO $ putResource localPool resource
      return ret
  inTransaction a = do
    withConnection \c -> liftIO $ withTransaction c a
    
insertUsers :: MonadDatabase m => [User] -> m ()
insertUsers users = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into users (id, username, passhash, creation_date) 
    values (?, ?, ?, ?);
   |] users

updateUserPasshash :: MonadDatabase m => Key User -> ByteString -> m Bool
updateUserPasshash user passhash = withConnection \c -> do
  dbUnique (lookupUsers [user]) >>= \case
    Just _ -> do
      True <$ (void . liftIO $ execute c [sql|
        update users
        set passhash = ?
        where users.id = ?; 
      |] (passhash, user))
    Nothing -> return False

data UpdateUsernameError = NewUsernameTaken

updateUsername :: (MonadReader (Pool Connection) m, MonadDatabase m) => User -> Text -> m (Maybe UpdateUsernameError)
updateUsername user@User{userID} newUsername = do
  pool <- ask
  liftIO $ withResource pool \c ->
    withTransaction c $ do
      dbUnique (lookupUsersByUsernameConn c [newUsername]) >>= \case
        Just user' -> if user' == user then pure Nothing else return $ Just NewUsernameTaken
        Nothing -> do
          void . liftIO $ execute c [sql|
            update users
            set username = ?
            where users.id = ?;
          |] (newUsername, userID)
          return Nothing

data ObjectAccessError = ObjectDoesntExist | RoleViolation | UserDoesntExist | UpdateSelfError

updateObjectContents :: MonadDatabase m => Key User -> Text -> ByteString -> m (Maybe ObjectAccessError)
updateObjectContents userID objectName contents = withConnection \c -> liftIO $ withTransaction c $
  dbUnique (lookupObjectsByNameConn c [objectName]) >>= \case
    Just Object{objectID} -> do
      maxPermissionLevelConn c userID objectID >>= \case
        Just role -> if role >= Edit then
          Nothing <$ void (execute c [sql|
            update objects
            set contents = ?
            where objects.name = ?;
            |] (objectName, contents))
          else
            return $ Just RoleViolation
        Nothing -> return $ Just RoleViolation
      return Nothing
    Nothing -> return $ Just ObjectDoesntExist

authedLookupObject :: MonadDatabase m => Key User -> Text -> m (Either ObjectAccessError Text)
authedLookupObject userID objectName = withConnection \c -> liftIO $ withTransaction c $
  dbUnique (lookupObjectsByNameConn c [objectName]) >>= \case
    Just Object{objectID, objectContents} -> do
      maxPermissionLevelConn c userID objectID >>= \case
        Just role ->
          if role >= Read then
            return $ Right (decodeUtf8 objectContents)
          else
            return $ Left RoleViolation
        Nothing -> return $ Left RoleViolation
    Nothing -> return $ Left ObjectDoesntExist

insertSessions :: MonadDatabase m => [Session] -> m ()
insertSessions sessions = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into sessions (id, owner, creation_date, token, expiration_date)
    values (?, ?, ?, ?, ?);
    |] sessions

insertObject :: MonadDatabase m => Key User -> Object -> m Bool
insertObject creator Object{..} = withConnection \c -> liftIO . withTransaction c $ do
  dbUnique (lookupObjectsByNameConn c [objectName]) >>= \case
    Just _ -> return False
    Nothing -> do
      void $ execute c [sql|
        insert into objects (id, name, contents, creation_date)
        values (?, ?, ?, ?); 
        insert into user_roles (user_, role, object, creation_date)
        values (?, ?, ?, ?);
      |] (objectID, objectName, objectContents, objectCreationDate, creator, Owner, objectID, objectCreationDate)
      return True

maxPermissionLevelConn :: Connection -> Key User -> Key Object -> IO (Maybe Role)
maxPermissionLevelConn c userID objectID = do
  userRoles <- fmap (fmap fromOnly) $ query c [sql| 
      select role from user_roles where user_ = ? and object = ?;
    |] (userID, objectID)
  sessionRoles <- fmap (fmap fromOnly) $ query c [sql|
      select role from session_roles 
      inner join sessions on sessions.owner = ? 
      where session_roles.session = sessions.id
        and session_roles.object = ?
        and sessions.expiration_date > now();
    |] (userID, objectID)
  return (aggregateRoles $ userRoles <> sessionRoles)
  where
    aggregateRoles :: [Role] -> Maybe Role
    aggregateRoles = go Nothing where
      go x [] = x
      go Nothing (role : roles) = go (Just role) roles
      go (Just role') (role : roles) = go (Just (max role role')) roles

maxPermissionLevel :: MonadDatabase m => Key User -> Key Object -> m (Maybe Role)
maxPermissionLevel userID objectID = do
  dbUnique (lookupObjects [objectID]) >>= \case
    Nothing -> return Nothing
    Just _ -> do
      withConnection \c -> liftIO $ maxPermissionLevelConn c userID objectID

lookupObjects :: MonadDatabase m => [Key Object] -> m [Object]
lookupObjects objects = withConnection \c -> do
  liftIO $ query c [sql|
    select * from objects where id in ?;
    |] (Only (In objects))

insertExecutedMigrations :: MonadDatabase m => [ExecutedMigration] -> m ()
insertExecutedMigrations executedMigrations = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into executed_migrations (id, file_name)
    values (?, ?);
  |] executedMigrations

reapSessions :: MonadDatabase m => m ()
reapSessions = withConnection \c -> do
  void . liftIO $ execute_ c [sql|
    delete from sessions where now() >= sessions.expiration_date;
    |]

validateToken :: MonadDatabase m => Text -> m (Maybe User)
validateToken token = withConnection \c -> do
  (liftIO $ query c [sql|
    select users.id, users.username, users.passhash, users.creation_date
    from users
    inner join sessions on owner=users.id
                        and token in ?
                      where sessions.expiration_date >= now();
    |] (Only (In [token]))) >>= \case
      [] -> do
        return Nothing
      [s] -> do
        return (Just s)
      _ -> error "database has two sessions with the same token" 
      
lookupUsersConn :: Connection -> [Key User] -> IO [User]
lookupUsersConn c users = do
  liftIO $ query c [sql|
     select * from users where id in ?;
    |] (Only (In users))

lookupUsers :: MonadDatabase m => [Key User] -> m [User]
lookupUsers users = withConnection \c -> liftIO $ lookupUsersConn c users

lookupUsersByUsernameConn :: Connection -> [Text] -> IO [User]
lookupUsersByUsernameConn c usernames = do
  query c [sql|
     select * from users where username in ?;
    |] (Only (In usernames))

lookupUsersByUsername :: MonadDatabase m => [Text] -> m [User]
lookupUsersByUsername usernames = withConnection \c -> do
  liftIO $ query c [sql|
     select * from users where username in ?;
    |] (Only (In usernames))

lookupObjectsByName :: MonadDatabase m => [Text] -> m [Object]
lookupObjectsByName names = withConnection \c -> do
  liftIO $ query c [sql|
    select * from objects where name in ?;
    |] (Only (In names))

lookupObjectsByNameConn :: Connection -> [Text] -> IO [Object]
lookupObjectsByNameConn c names = do
  query c [sql|
    select * from objects where name in ?;
    |] (Only (In names))

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

dbUnique :: (Typeable a, Monad m) => m [a] -> m (Maybe a)
dbUnique (as :: m [a]) = as >>= \case
  [a] -> return $ Just a
  [] -> return $ Nothing
  _ -> error $ "Found more than one " <> show (typeRep (Proxy @a)) <> " in the database"

updateUserRole :: MonadDatabase m => User -> Text -> Text -> Role -> m (Maybe ObjectAccessError)
updateUserRole User{userID, username} targetUsername objectName role = 
  if username == targetUsername 
  then return $ Just UpdateSelfError 
  else withConnection \c -> liftIO $ withTransaction c $ do
    dbUnique (lookupObjectsByNameConn c [objectName]) >>= \case
      Just Object{objectID} -> do
        dbUnique (lookupUsersByUsernameConn c [targetUsername]) >>= \case
          Just (User userID' _ _ _) -> do
            perm <- maxPermissionLevelConn c userID objectID
            if (perm == Just Collaborator && role < Collaborator
             || perm >= Just Owner        && role < Owner) then do
              userRole :: Maybe Role <- fmap (listToMaybe . fmap fromOnly) $ query c [sql| 
                  select role from user_roles 
                  inner join objects on objects.name = ?
                  where user_ = ? and object = objects.id;
                |] (userID, objectName)
              case userRole of
                Nothing -> Nothing <$ void (execute c [sql|
                    insert into user_roles (user_, role, object, creation_date)
                    values (?, ?, ?, now());
                  |] (userID', role, objectID))
                Just _ -> Nothing <$ void (execute c [sql|
                    update user_roles
                    set role = ?
                    where user_ = ? and object = ?;
                  |] (role, userID', objectID))
            else return $ Just RoleViolation
          Nothing -> return $ Just UserDoesntExist
      Nothing -> return $ Just ObjectDoesntExist 
