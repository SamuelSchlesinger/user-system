module Freeze.Monad (MonadFreeze) where

import Control.Monad.Catch 
  ( MonadCatch )
import Control.Monad.Except 
  ( MonadError )
import Servant
  ( ServerError )
import Freeze.Database
  ( MonadDatabase )
import Control.Monad.Reader 
  ( MonadReader )
import Data.Pool
  ( Pool )
import Database.PostgreSQL.Simple
  ( Connection )

type MonadFreeze m = (MonadDatabase m, MonadError ServerError m, MonadCatch m, MonadReader (Pool Connection) m)
