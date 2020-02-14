module UserSystem.Monad where

import Control.Monad.Catch
import Control.Monad.Except
import Servant
import UserSystem.Database

type MonadUserSystem m = (MonadDatabase m, MonadError ServerError m, MonadCatch m)
