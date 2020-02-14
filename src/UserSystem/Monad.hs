module UserSystem.Monad where

import Control.Monad.Catch
import Control.Monad.Except
import Servant
import UserSystem.Database
import Control.Monad.Reader
import Data.Pool
import Database.PostgreSQL.Simple

type MonadUserSystem m = (MonadDatabase m, MonadError ServerError m, MonadCatch m, MonadReader (Pool Connection) m)
