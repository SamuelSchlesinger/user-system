{-# LANGUAGE DeriveAnyClass #-}

module UserSystem.API where

import Servant
import Servant.Server.Experimental.Auth
import UserSystem.API.Types
import UserSystem.Ontology
import qualified Network.Wai as Wai

type UserSystemAPI
     = "account" :> AccountAPI 
  :<|> Raw

type SimpleReqRes req res = ReqBody '[JSON, FormUrlEncoded] req :> Post '[JSON] res

type SimpleResWithHeaders res = Post '[JSON] (WithCookieHeaders res)

type SimpleReq req = SimpleReqRes req (Response req)

type AccountAPI
     = "signup" :> SimpleReqRes SignUp (Response SignUp)
  :<|> "signin" :> SimpleReqRes SignIn (WithCookieHeaders (Response SignIn)) 
  :<|> AuthenticatedAccountAPI
  
type AuthenticatedAccountAPI
    = AuthProtect "user" :> 
     ( "new-token" :> SimpleResWithHeaders (Response SignIn)
 :<|> "change-password" :> SimpleReqRes ChangePassword (Response ChangePassword)
 :<|> "change-username" :> SimpleReqRes ChangeUsername (Response ChangeUsername)
 :<|> "create-object" :> SimpleReqRes CreateObject (Response CreateObject)
 :<|> "edit-object" :> SimpleReqRes EditObject (Response EditObject)
 :<|> "read-object" :> SimpleReqRes ReadObject (Response ReadObject)
 :<|> "give-user-role" :> SimpleReqRes GiveUserRole (Response GiveUserRole)
     )

type Ctx = AuthHandler Wai.Request User ': '[]

type instance AuthServerData (AuthProtect "user") = User
