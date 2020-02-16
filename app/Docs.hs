{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Lens
import Data.Proxy
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Void
import UserSystem.API.Types
import UserSystem.API
import UserSystem.Ontology
import Servant
import Servant.Docs hiding (Response)
import Web.Cookie (SetCookie)

instance ToSample ()
instance ToSample (Key User) where toSamples _ = [("Sam's username", Key "samuel")]
instance ToSample (Response SignIn) where toSamples _ = []
instance ToSample (Response SignUp) where toSamples _ = []
instance ToSample (Response ChangePassword) where toSamples _ = []
instance ToSample (Response ChangeUsername) where toSamples _ = []
instance ToSample (Response CreateObject) where toSamples _ = []
instance ToSample (Response EditObject) where toSamples _ = []
instance ToSample (Response ReadObject) where toSamples _ = []
instance ToSample (Response GiveUserRole) where toSamples _ = []
instance ToSample GiveUserRole where toSamples _ = []
instance ToSample EditObject where toSamples _ = []
instance ToSample ReadObject where toSamples _ = []
instance ToSample SetCookie where toSamples _ = []
instance ToSample SignIn where toSamples _ = [("Samuel Schlesinger", SignIn "samuel" "password")]
instance ToSample SignUp where toSamples _ = [("Samuel Schlesinger", SignUp "samuel" "password")]
instance ToSample ChangePassword where toSamples _ = [("Samuel Schlesinger", ChangePassword "new-password")]
instance ToSample ChangeUsername where toSamples _ = [("Samuel Schlesinger", ChangeUsername "new-username")]
instance ToSample CreateObject where toSamples _ = [("Samuel Schlesinger", CreateObject "object name" "great new object")]
instance ToSample UTCTime where toSamples _ = []
instance ToSample Char where toSamples _ = [(pack [x], x) | x <- ['a' .. 'z']]

instance HasDocs p => HasDocs (AuthProtect "user" :> p) where
  docsFor Proxy (e, a) = docsFor (Proxy @p) (e, a & authInfo %~ (<> [geoAuthDoc])) where
    geoAuthDoc :: DocAuthentication
    geoAuthDoc = DocAuthentication "A simple HTTP-only session based authentication system" "A session cookie"

main :: IO ()
main = putStrLn . markdown . docs $ Proxy @UserSystemAPI
