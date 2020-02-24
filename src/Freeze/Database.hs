{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Freeze.Database where

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
  , ToRow
  , query_
  , Query
  , Only(..) 
  , In(..) )
import Database.PostgreSQL.Simple.SqlQQ
  ( sql )
import Freeze.Ontology
  ( User(..)
  , Key(..)
  , Session(..)
  , Role(..)
  , ExecutedMigration(..) 
  , Box(..)
  , MousePropertyType(..) 
  , MouseProperty(..)
  , SampleType(..)
  , Sample(..)
  , Freezer(..)
  , FullFreezer(..)
  , FullShelf(..)
  , FullRack(..)
  , FullBox(..)
  , Project(..)
  , Mouse(..)
  )
import Data.Typeable
  ( Typeable, typeRep, Proxy(..) )
import qualified Data.Map as Map
import Control.Monad
  ( forM )

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

data BoxAccessError = BoxDoesntExist | RoleViolation | UserDoesntExist | UpdateSelfError

updateBoxContents :: MonadDatabase m => Key User -> Text -> ByteString -> m (Maybe BoxAccessError)
updateBoxContents userID boxName contents = withConnection \c -> liftIO $ withTransaction c $
  dbUnique (lookupBoxsByNameConn c [boxName]) >>= \case
    Just Box{boxID} -> do
      maxPermissionLevelConn c userID boxID >>= \case
        Just role -> if role >= Edit then
          Nothing <$ void (execute c [sql|
            update boxs
            set contents = ?
            where boxs.name = ?;
            |] (boxName, contents))
          else
            return $ Just RoleViolation
        Nothing -> return $ Just RoleViolation
      return Nothing
    Nothing -> return $ Just BoxDoesntExist

authedLookupBox :: MonadDatabase m => Key User -> Text -> m (Either BoxAccessError FullBox)
authedLookupBox userID boxName = withConnection \c -> liftIO $ withTransaction c $
  dbUnique (lookupBoxesByNameConn c [boxName]) >>= \case
    Just box@Box{boxID} -> do
      maxPermissionLevelConn c userID boxID >>= \case
        Just role ->
          if role >= Read then
            Right <$> lookupFullBoxConn c boxID
          else
            return $ Left RoleViolation
        Nothing -> return $ Left RoleViolation
    Nothing -> return $ Left BoxDoesntExist

insertSessions :: MonadDatabase m => [Session] -> m ()
insertSessions sessions = withConnection \c -> do
  void . liftIO $ executeMany c [sql|
    insert into sessions (id, owner, creation_date, token, expiration_date)
    values (?, ?, ?, ?, ?);
    |] sessions

insertBox :: MonadDatabase m => Key User -> Box -> m Bool
insertBox creator Box{..} = withConnection \c -> liftIO . withTransaction c $ do
  dbUnique (lookupBoxsByNameConn c [boxName]) >>= \case
    Just _ -> return False
    Nothing -> do
      void $ execute c [sql|
        insert into boxs (id, name, contents, creation_date)
        values (?, ?, ?, ?); 
        insert into user_roles (user_, role, box, creation_date)
        values (?, ?, ?, ?);
      |] (boxID, boxName, boxContents, boxCreationDate, creator, Owner, boxID, boxCreationDate)
      return True

maxPermissionLevelConn :: Connection -> Key User -> Key Box -> IO (Maybe Role)
maxPermissionLevelConn c userID boxID = do
  userRoles <- fmap (fmap fromOnly) $ query c [sql| 
      select role from user_roles where user_ = ? and box = ?;
    |] (userID, boxID)
  sessionRoles <- fmap (fmap fromOnly) $ query c [sql|
      select role from session_roles 
      inner join sessions on sessions.owner = ? 
      where session_roles.session = sessions.id
        and session_roles.box = ?
        and sessions.expiration_date > now();
    |] (userID, boxID)
  return (aggregateRoles $ userRoles <> sessionRoles)
  where
    aggregateRoles :: [Role] -> Maybe Role
    aggregateRoles = go Nothing where
      go x [] = x
      go Nothing (role : roles) = go (Just role) roles
      go (Just role') (role : roles) = go (Just (max role role')) roles

maxPermissionLevel :: MonadDatabase m => Key User -> Key Box -> m (Maybe Role)
maxPermissionLevel userID boxID = do
  dbUnique (lookupBoxs [boxID]) >>= \case
    Nothing -> return Nothing
    Just _ -> do
      withConnection \c -> liftIO $ maxPermissionLevelConn c userID boxID

lookupBoxesByNameConn :: Connection -> [Text] -> IO [Box]
lookupBoxesByNameConn c boxs = do
  query c [sql|
    select * from boxes where name in ?;
    |] (Only (In boxs))

lookupFullBoxConn :: Connection -> Key Box -> IO FullBox
lookupFullBoxConn c boxID = _
  

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

lookupBoxsByName :: MonadDatabase m => [Text] -> m [Box]
lookupBoxsByName names = withConnection \c -> do
  liftIO $ query c [sql|
    select * from boxs where name in ?;
    |] (Only (In names))

lookupBoxsByNameConn :: Connection -> [Text] -> IO [Box]
lookupBoxsByNameConn c names = do
  query c [sql|
    select * from boxs where name in ?;
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

updateUserRole :: MonadDatabase m => User -> Text -> Text -> Role -> m (Maybe BoxAccessError)
updateUserRole User{userID, username} targetUsername boxName role = 
  if username == targetUsername 
  then return $ Just UpdateSelfError 
  else withConnection \c -> liftIO $ withTransaction c $ do
    dbUnique (lookupBoxsByNameConn c [boxName]) >>= \case
      Just Box{boxID} -> do
        dbUnique (lookupUsersByUsernameConn c [targetUsername]) >>= \case
          Just (User userID' _ _ _) -> do
            perm <- maxPermissionLevelConn c userID boxID
            if (perm == Just Collaborator && role < Collaborator
             || perm >= Just Owner        && role < Owner) then do
              userRole :: Maybe Role <- fmap (listToMaybe . fmap fromOnly) $ query c [sql| 
                  select role from user_roles 
                  inner join boxs on boxs.name = ?
                  where user_ = ? and box = boxs.id;
                |] (userID, boxName)
              case userRole of
                Nothing -> Nothing <$ void (execute c [sql|
                    insert into user_roles (user_, role, box, creation_date)
                    values (?, ?, ?, now());
                  |] (userID', role, boxID))
                Just _ -> Nothing <$ void (execute c [sql|
                    update user_roles
                    set role = ?
                    where user_ = ? and box = ?;
                  |] (role, userID', boxID))
            else return $ Just RoleViolation
          Nothing -> return $ Just UserDoesntExist
      Nothing -> return $ Just BoxDoesntExist 

sqlMany :: (MonadDatabase m, ToRow q) => Query -> [q] -> m ()
sqlMany q things = withConnection \c -> void . liftIO $ executeMany c q things

insertBoxes :: MonadDatabase m => [Box] -> m ()
insertBoxes = sqlMany 
  [sql|
    insert into boxes (id, code, project, freezer, shelf, rack, slot_x, slot_y, creation_date)
    values (?, ?, ?, ?, ?, ?, ?, ?, ?);
  |]

insertMousePropertyTypes :: MonadDatabase m => [MousePropertyType] -> m ()
insertMousePropertyTypes = sqlMany
  [sql|
    insert into mouse_property_types (name, description, creation_date)
    values (?, ?, ?);
  |]

insertMouseProperties :: MonadDatabase m => [MouseProperty] -> m ()
insertMouseProperties = sqlMany
  [sql|
    insert into mouse_property (property_type, name, description, creation_date)
    values (?, ?, ?, ?);
  |]

insertProjects :: MonadDatabase m => [Project] -> m ()
insertProjects = sqlMany
  [sql|
    insert into projects (id, name, description, creation_date)
    values (?, ?, ?, ?);
  |]

insertSampleTypes :: MonadDatabase m => [SampleType] -> m ()
insertSampleTypes = sqlMany
  [sql|
    insert into sample_types (name, description, creation_date)
    values (?, ?, ?);
  |]

insertMice :: MonadDatabase m => [Mouse] -> m ()
insertMice = sqlMany
  [sql|
    insert into mice (id, tag, date_of_birth, box, date_of_tissue_collection, creation_date)
    values (?, ?, ?, ?, ?, ?);
  |]

insertSamples :: MonadDatabase m => [Sample] -> m ()
insertSamples = sqlMany
  [sql|
    insert into samples (id, code, mouse, box, sample_type, notes, creation_date)
    values (?, ?, ?, ?, ?, ?, ?);
  |]

insertFreezers :: MonadDatabase m => [Freezer] -> m ()
insertFreezers = sqlMany
  [sql|
    insert into freezers (id, code, name, rack_width, rack_height, racks_per_shelf, shelf_number, creation_date)
    values (?, ?, ?, ?, ?, ?, ?, ?);
  |]

lookupAllUsers :: MonadDatabase m => m [User]
lookupAllUsers = withConnection \c -> do
  liftIO $ query_ c [sql|
     select * from users;
    |]

  
lookupAllExecutedMigrations :: MonadDatabase m => m [ExecutedMigration]
lookupAllExecutedMigrations = withConnection \c -> do
  liftIO $ query_ c [sql|
    select * from executed_migrations;
  |]

lookupFreezersByNames :: MonadDatabase m => [Text] -> m [Freezer]
lookupFreezersByNames names = withConnection \c -> do
  liftIO $ query c [sql|
    select * from freezers where name in ?;
  |] (Only (In names))

lookupFreezers :: MonadDatabase m => [Key Freezer] -> m [Freezer]
lookupFreezers freezers = withConnection \c -> do
  liftIO $ query c [sql|
    select * from freezers where id in ?;
  |] (Only (In freezers))

lookupBoxes :: MonadDatabase m => [Key Box] -> m [Box]
lookupBoxes boxes = withConnection \c -> do
  liftIO $ query c [sql|
    select * from boxes where id in ?;
  |] (Only (In boxes))

lookupMouseProperties :: MonadDatabase m => Key MousePropertyType -> [Key MouseProperty] -> m [MouseProperty]
lookupMouseProperties mousePropertyType mouseProperties = withConnection \c -> do
  liftIO $ query c [sql|
    select * from mouse_properties where id in ? and property_type = ?;
  |] (In mouseProperties, mousePropertyType)

lookupSampleTypes :: MonadDatabase m => [Key SampleType] -> m [SampleType]
lookupSampleTypes sampleTypes = withConnection \c -> do
  liftIO $ query c [sql|
    select * from sample_types where name in ?;
  |] (Only (In sampleTypes))

lookupMice :: MonadDatabase m => [Key Mouse] -> m [Mouse]
lookupMice mice = withConnection \c -> do
  liftIO $ query c [sql|
    select * from mice where id in ?;
  |] (Only (In mice))

lookupAllFullFreezers :: MonadDatabase m => m [FullFreezer]
lookupAllFullFreezers = lookupAllFreezers >>= mapM fillFreezer

fillFreezer :: MonadDatabase m => Freezer -> m FullFreezer
fillFreezer freezer@Freezer{..} = do
  boxes <- boxesOfFreezer freezerID
  let byIndex = [ (boxShelf, ((boxSlotX, boxSlotY), box)) | box@Box{..} <- boxes ]
  decorated <- forM byIndex \(boxShelf, ((boxSlotX, boxSlotY), box@Box{boxID, boxRack})) -> do
    samples <- samplesOfBox boxID
    return (boxShelf, [(boxRack, [((boxSlotX, boxSlotY), FullBox box samples)])])
  return . FullFreezer freezer
         $ Map.map (FullShelf . Map.map (FullRack . Map.fromList))
         $ Map.map (Map.fromListWith (++)) 
         $ Map.fromListWith (++) decorated

boxesOfFreezer :: MonadDatabase m => Key Freezer -> m [Box]
boxesOfFreezer freezer = withConnection \c -> do
  liftIO $ query c [sql|
    select * from boxes where freezer = ?;
    |] (Only freezer)

samplesOfBox :: MonadDatabase m => Key Box -> m [Sample]
samplesOfBox box = withConnection \c -> do
  liftIO $ query c [sql|
    select * from samples where box = ?;
    |] (Only box)

samplesOfBoxConn :: Connection -> Key Box -> IO [Sample]
samplesOfBox c box = do
  query c [sql|
    select * from samples where box = ?;
    |] (Only box)

lookupFullFreezers :: MonadDatabase m => [Key Freezer] -> m [FullFreezer]
lookupFullFreezers freezers = lookupFreezers freezers >>= mapM fillFreezer

lookupProjectsByNames :: MonadDatabase m => [Text] -> m [Freezer]
lookupProjectsByNames names = withConnection \c -> do
  liftIO $ query c [sql|
    select * from projects where name in ?;
  |] (Only (In names))

lookupAllFreezers :: MonadDatabase m => m [Freezer]
lookupAllFreezers = withConnection \c -> do
  liftIO $ query_ c [sql|
    select * from freezers;
  |]

deleteBoxes :: MonadDatabase m => [Key Box] -> m Int
deleteBoxes boxes = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
    delete from boxes where id in ?;
    |] (Only (In boxes))

deleteMousePropertyTypes :: MonadDatabase m => [Key MousePropertyType] -> m Int
deleteMousePropertyTypes mousePropertyTypes = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
    delete from mouse_property_types where name in ?;
    |] (Only (In mousePropertyTypes))

deleteMouseProperties :: MonadDatabase m => Key MousePropertyType -> [Key MouseProperty] -> m Int
deleteMouseProperties mousePropertyType mouseProperties = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
    delete from mouse_properties where name in ? and property_type = ?;
    |] (In mouseProperties, mousePropertyType)

deleteProjects :: MonadDatabase m => [Key Project] -> m Int
deleteProjects projects = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
    delete from projects where id in ?;
    |] (Only (In projects))

deleteSampleTypes :: MonadDatabase m => [Key SampleType] -> m Int
deleteSampleTypes sampleTypes = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
    delete from sample_types where name in ?;
    |] (Only (In sampleTypes))

deleteSamples :: MonadDatabase m => [Key Sample] -> m Int
deleteSamples samples = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
    delete from samples where id in ?;
    |] (Only (In samples))

deleteMice :: MonadDatabase m => [Key Mouse] -> m Int
deleteMice mice = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
    delete from mice where id in ?;
    |] (Only (In mice))

deleteFreezers :: MonadDatabase m => [Key Freezer] -> m Int
deleteFreezers freezers = withConnection \c -> do
  fmap fromIntegral . liftIO $ execute c [sql|
    delete from freezers where id in ?;
    |] (Only (In freezers))
