module UserSystem.Monad where

import Servant
import Control.Monad.Except
import Control.Monad.Catch
import UserSystem.Database

type MonadUserSystem m = (MonadDatabase m, MonadError ServerError m, MonadCatch m)
