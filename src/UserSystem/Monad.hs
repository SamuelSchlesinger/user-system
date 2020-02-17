module UserSystem.Monad (MonadUserSystem) where

import Control.Monad.Catch 
  ( MonadCatch )
import Control.Monad.Except 
  ( MonadError )
import Servant
  ( ServerError )
import UserSystem.Database
  ( MonadDatabase )
import Control.Monad.Reader 
  ( MonadReader )
import Data.Pool
  ( Pool )
import Database.PostgreSQL.Simple
  ( Connection )

type MonadUserSystem m = (MonadDatabase m, MonadError ServerError m, MonadCatch m, MonadReader (Pool Connection) m)
