{-# LANGUAGE DeriveAnyClass #-}

module UserSystem.API where

import Servant
import Servant.Server.Experimental.Auth
import UserSystem.API.Types
import UserSystem.Ontology
import Web.Cookie
import qualified Network.Wai as Wai

type UserSystemAPI
     = "account" :> AccountAPI 
  :<|> Raw

type SimplePost req res = ReqBody '[JSON, FormUrlEncoded] req :> Post '[JSON] res

type SimplePostWithHeaders res = Post '[JSON] (WithCookieHeaders res)

type AccountAPI
     = "signup" :> SimplePost SignUp (Response SignUp)
  :<|> "signin" :> SimplePost SignIn (WithCookieHeaders (Response SignIn)) 
  :<|> AuthenticatedAccountAPI
  
type AuthenticatedAccountAPI
    = AuthProtect "user" :> 
     ( "new-token" :> SimplePostWithHeaders (Response SignIn)
 :<|> "change-password" :> SimplePost ChangePassword (Response ChangePassword)
 :<|> "change-username" :> SimplePost ChangeUsername (Response ChangeUsername)
 :<|> "create-object" :> SimplePost CreateObject (Response CreateObject)
 :<|> "edit-object" :> SimplePost EditObject (Response EditObject)
 :<|> "read-object" :> SimplePost ReadObject (Response ReadObject)
     )

type Ctx = AuthHandler Wai.Request User ': '[]

type instance AuthServerData (AuthProtect "user") = User
