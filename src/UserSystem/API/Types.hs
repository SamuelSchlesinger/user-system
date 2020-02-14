module UserSystem.API.Types where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Web.FormUrlEncoded

data family Response a

type family Request b where
  Request (Response a) = a

data SignUp = SignUp
  { signUpUsername :: Text
  , signUpPassword :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response SignUp = SignedUp
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SignIn = SignIn
  { signInUsername :: Text
  , signInPassword :: Text }
  deriving  (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response SignIn = SignedIn
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChangePassword = ChangePassword
  { changePasswordNewPassword :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response ChangePassword = ChangedPassword
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChangeUsername = ChangeUsername
  { changeUsernameNewUsername :: Text }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON, ToForm, FromForm)

data instance Response ChangeUsername = ChangedUsername
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)
