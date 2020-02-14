{-# LANGUAGE DeriveAnyClass #-}

module UserSystem.API where

import UserSystem.Ontology
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie
import qualified Network.Wai as Wai
import UserSystem.API.Types

type UserSystemAPI
     = "account" :> AccountAPI 
  :<|> Raw

type SimplePost req res = ReqBody '[JSON, FormUrlEncoded] req :> Post '[JSON] res

type SimplePostWithHeaders res = Post '[JSON] (WithCookieHeaders res)

type WithCookieHeaders res
   = Headers '[ Header "Set-Cookie" SetCookie
              , Header "Access-Control-Allow-Origin" String
              , Header "Access-Control-Allow-Headers" String
              , Header "Access-Control-Allow-Credentials" Bool ] res

type AccountAPI
     = "signup" :> SimplePost SignUp (Response SignUp)
  :<|> "signin" :> SimplePost SignIn (WithCookieHeaders (Response SignIn)) 
  :<|> AuthProtect "user" :> AuthorizedAccountAPI
  
type AuthorizedAccountAPI
    = "new-token" :> SimplePostWithHeaders (Response SignIn)
 :<|> "change-password" :> SimplePost ChangePassword (Response ChangePassword)
 :<|> "change-username" :> SimplePost ChangeUsername (Response ChangeUsername)

type Ctx = AuthHandler Wai.Request User ': '[]

type instance AuthServerData (AuthProtect "user") = User
